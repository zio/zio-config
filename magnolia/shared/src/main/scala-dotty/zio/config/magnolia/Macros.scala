package zio.config.magnolia

import scala.quoted.*
import zio.config.derivation._

/**
 * Compile-time helpers for Scala 3 derivation: they read the derivation annotations and the default values of a type.
 */
object Macros:
  /** The `@name` annotations on `T`. */
  inline def nameOf[T]: List[name] = ${ anns[T, name]("zio.config.derivation.name") }

  /** The `@discriminator` annotations on `T`. */
  inline def discriminator[T]: List[discriminator] = ${ anns[T, discriminator]("zio.config.derivation.discriminator") }

  /** The `@describe` annotations on `T`. */
  inline def documentationOf[T]: List[describe] = ${ anns[T, describe]("zio.config.derivation.describe") }

  /** The `@name` annotations of each field of `T` that has one. */
  inline def fieldNameOf[T]: List[(String, List[name])] = ${ fieldAnns[T, name]("zio.config.derivation.name") }

  /** The `@describe` annotations of each field of `T` that has one. */
  inline def fieldDocumentationOf[T]: List[(String, List[describe])] = ${
    fieldAnns[T, describe]("zio.config.derivation.describe")
  }

  /** The default values of the fields of `T`, by field name. */
  inline def defaultValuesOf[T]: List[(String, Any)] = ${ defaultValues[T] }

  /** The macro implementation of `defaultValuesOf`. */
  def defaultValues[T: Type](using Quotes): Expr[List[(String, Any)]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val sym = tpe.typeSymbol

    val namesOfFieldsWithDefaultValues =
      sym.caseFields.filter(s => s.flags.is(Flags.HasDefault)).map(_.name)

    val companionClas =
      sym.companionClass

    val defaultRefs =
      companionClas.declarations
        .filter(_.name.startsWith("$lessinit$greater$default"))
        .map(Ref(_))

    Expr.ofList(namesOfFieldsWithDefaultValues.zip(defaultRefs).map { case (n, ref) =>
      Expr.ofTuple(Expr(n), ref.asExpr)
    })

  /** Returns the annotations on `T` whose class has the full name `ownerName`. */
  def anns[T: Type, A: Type](ownerName: String)(using Quotes): Expr[List[A]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a =>
        a.tpe.typeSymbol.fullName == ownerName
      }.map(_.asExpr.asInstanceOf[Expr[A]])
    }
  }

  /**
   * Returns, for each field of `T`, the annotations whose class has the full name `ownerName`; fields without one are
   * left out.
   */
  def fieldAnns[T: Type, A: Type](ownerName: String)(using Quotes): Expr[List[(String, List[A])]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.fullName == ownerName
        }.map(_.asExpr.asInstanceOf[Expr[A]])
      }.filter(_._2.nonEmpty).map((name, anns) => Expr.ofTuple(name, Expr.ofList(anns)))
    }
end Macros
