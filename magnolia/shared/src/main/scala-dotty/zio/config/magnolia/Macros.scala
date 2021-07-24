package zio.config.magnolia

import scala.quoted.*

object Macros:
  inline def nameOf[T]: List[name] = ${anns[T, name]("zio.config.magnolia.name")}
  inline def namesOf[T]: List[names] = ${anns[T, names]("zio.config.magnolia.names")}
  inline def documentationOf[T]: List[describe] = ${anns[T, describe]("zio.config.magnolia.describe")}
  inline def fieldNameOf[T]: List[(String, List[name])] = ${fieldAnns[T, name]("zio.config.magnolia.name")}
  inline def fieldNamesOf[T]: List[(String, List[names])] = ${fieldAnns[T, names]("zio.config.magnolia.names")}
  inline def fieldDocumentationOf[T]: List[(String, List[describe])] = ${fieldAnns[T, describe]("zio.config.magnolia.describe")}
  inline def defaultValuesOf[T]: List[(String, Any)] = ${defaultValues[T]}

  def defaultValues[T : Type](using Quotes): Expr[List[(String, Any)]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val sym = tpe.typeSymbol

    val namesOfFieldsWithDefaultValues =
      sym.caseFields.filter(s => s.flags.is(Flags.HasDefault)).map(_.name)

    val companionClas =
      sym.companionClass

    // Compiler throws horrible, if no specific checks
    val tree =
      if (companionClas.isClassDef) {
        Some(companionClas.tree.asInstanceOf[ClassDef].body)
      } else {
        None
      }

    val defaultRefs: List[Ref] =
      tree match {
        case Some(body) =>
          body.collect {
            case df @ DefDef(name, _, _, _)  if name.startsWith("$lessinit$greater$default") =>
              Ref(df.symbol)
          }
        case None => Nil
      }

    Expr.ofList(namesOfFieldsWithDefaultValues.zip(defaultRefs).map {
      case (n, ref) => Expr.ofTuple(Expr(n), ref.asExpr)
    })

  def anns[T: Type, A: Type](ownerName: String)(using Quotes): Expr[List[A]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a => {
         a.tpe.typeSymbol.fullName == ownerName
      }}.map(_.asExpr.asInstanceOf[Expr[A]])
    }
  }

  def fieldAnns[T: Type, A: Type](ownerName: String)(using Quotes): Expr[List[(String, List[A])]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.fullName == ownerName
        }.map(_.asExpr.asInstanceOf[Expr[A]])
      }.filter(_._2.nonEmpty).map { (name, anns) => Expr.ofTuple(name, Expr.ofList(anns)) }
    }
end Macros