package zio.config

import scala.annotation.nowarn
import scala.reflect.macros.whitebox

trait TupleConversion[A, B] {
  def to(a: A): B
  def from(b: B): A
}

object TupleConversion        {
  def apply[P, T]: TupleConversion[P, T] = macro genTupleConversion[P, T]

  @nowarn("msg=evidence parameter")
  def genTupleConversion[P: c.WeakTypeTag, T: c.WeakTypeTag](
    c: whitebox.Context
  ): c.Expr[TupleConversion[P, T]] = {
    import c.universe._
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
trait ImplicitTupleConversion {
  implicit def autoTupleConversion[P <: Product, T]: TupleConversion[P, T] =
    macro TupleConversion.genTupleConversion[P, T]

  def autoTupleConversion1[P, A](implicit ev: TupleConversion[P, Tuple1[A]]): TupleConversion[P, A] =
    ??? // defined here so it can be used in explicit imports when cross-compiling
}
