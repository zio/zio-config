package zio.config

import scala.reflect.macros.whitebox

/**
 * Converts between a case class `A` and `B`, the tuple of its field types. `Config#to` uses it to turn a config of a
 * tuple into a config of a case class. `ImplicitTupleConversion` derives instances for case classes: on Scala 2 with a
 * macro, on Scala 3 from the case class's `Mirror`.
 *
 * {{{
 * import zio.config._
 *
 * final case class Endpoint(host: String, port: Int)
 *
 * val conversion = implicitly[TupleConversion[Endpoint, (String, Int)]]
 *
 * val fields: (String, Int) = conversion.to(Endpoint("api.example.com", 443))
 * // (api.example.com,443)
 * }}}
 */
trait TupleConversion[A, B] {

  /**
   * Returns the fields of `a` as a `B`.
   */
  def to(a: A): B

  /**
   * Builds an `A` from the fields in `b`.
   */
  def from(b: B): A
}

/**
 * Companion of [[TupleConversion]].
 */
object TupleConversion {

  /**
   * Derives the `TupleConversion` for the case class `P` with a macro. The macro picks the target type from the fields
   * of `P`: the field's type for one field, a tuple for more. For a type that isn't a case class, compilation fails;
   * for `class Plain(val a: Int)` the message is `Type class Plain is not a case class`.
   */
  def apply[P, T]: TupleConversion[P, T] = macro genTupleConversion[P, T]

  /**
   * The macro implementation behind `apply` and `autoTupleConversion`. It isn't meant to be called directly.
   */
  def genTupleConversion[P: c.WeakTypeTag, T: c.WeakTypeTag](
    c: whitebox.Context
  ): c.Expr[TupleConversion[P, T]] = {
    import c.universe._
    implicitly[c.WeakTypeTag[T]]
    // silence unused parameter warning.
    // we can't use a @nowarn annotation because the WeakTypeTag is needed
    // on scala 2.12 and we would therefore get a
    // "@nowarn annotation does not suppress any warnings" error on that version
    val prodTpe    = c.weakTypeOf[P]
    if (
      !prodTpe.typeSymbol.isClass ||
      !prodTpe.typeSymbol.asClass.isCaseClass
    ) {
      c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} is not a case class")
    }
    val paramLists = prodTpe.typeSymbol.asClass.primaryConstructor.asMethod.typeSignatureIn(prodTpe).paramLists
    val result     = paramLists match {
      case List(List(singleParam)) =>
        // Special case: single parameter products

        val typ = singleParam.typeSignatureIn(prodTpe).finalResultType

        q"""new _root_.zio.config.TupleConversion[$prodTpe, $typ] {
                override def to(a: $prodTpe): $typ = a.${singleParam.name.toTermName}                
                override def from(b: $typ): $prodTpe =
                    new ${prodTpe}(b)
        }
        """

      case List(params) =>
        // Generic case: n > 1 parameters

        val tupleName   = definitions.TupleClass(params.size).name.toTypeName
        val tupleParams = params.map { sym =>
          sym.typeSignatureIn(prodTpe).finalResultType
        }
        val tup         = tq"$tupleName[..$tupleParams]"
        val packers     =
          params.map { sym =>
            val symTerm = sym.name.toTermName
            q"a.$symTerm"
          }
        val unpackers   =
          params.indices.map { idx =>
            val accessor = TermName(s"_${idx + 1}")
            q"b.$accessor"
          }

        q"""new _root_.zio.config.TupleConversion[$prodTpe, $tup] {
                override def to(a: $prodTpe): $tup =
                (..$packers)
                override def from(b: $tup): $prodTpe =
                new ${prodTpe}(..$unpackers)
            }
            """
      case Nil          =>
        // Special case: zero parameter products
        q"""new _root_.zio.config.TupleConversion[$prodTpe, Unit] {
                override def to(a: $prodTpe): Unit = ()
                override def from(b: Unit): $prodTpe =
                    new $prodTpe()
            }
        """
      case _            =>
        c.abort(
          c.enclosingPosition,
          s"Type ${prodTpe.typeSymbol} has multiple parameter lists which is currently not supported"
        )
    }

    c.Expr(result)
  }
}

/**
 * Provides the `TupleConversion` instances for case classes. The `zio.config` package object extends it, so
 * `import zio.config._` brings them into scope.
 */
trait ImplicitTupleConversion {

  /**
   * Derives a `TupleConversion` from a case class to the tuple of its field types. On Scala 2, a case class with one
   * field converts to that field's type. On Scala 3, it converts to a `Tuple1`, and `autoTupleConversion1` gives the
   * conversion to the field's type.
   */
  implicit def autoTupleConversion[P <: Product, T]: TupleConversion[P, T] =
    macro TupleConversion.genTupleConversion[P, T]

  /**
   * On Scala 3, turns a conversion to `Tuple1[A]` into a conversion to `A`, so a case class with one field converts to
   * that field's type. On Scala 2 it is not implicit and has no implementation: it exists so that code built for both
   * versions can import it by name, and calling it throws `scala.NotImplementedError`.
   */
  def autoTupleConversion1[P, A](implicit ev: TupleConversion[P, Tuple1[A]]): TupleConversion[P, A] =
    ??? // defined here so it can be used in explicit imports when cross-compiling
}
