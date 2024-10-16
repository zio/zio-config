package zio.config.magnolia

import scala.quoted.*
import zio.config.derivation._

private[magnolia] object DefaultValueMacros:

  inline def defaultValuesOf[T]: List[(String, Any)]                  = ${ defaultValues[T] }
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

end DefaultValueMacros
