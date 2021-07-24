package zio.config.magnolia

import scala.quoted.*

object Macros:
  inline def nameOf[T]: List[name] = ${anns[T, name]("zio.config.magnolia.name")}
  inline def namesOf[T]: List[names] = ${anns[T, names]("zio.config.magnolia.names")}
  inline def documentationOf[T]: List[describe] = ${anns[T, describe]("zio.config.magnolia.describe")}
  inline def fieldNameOf[T]: List[(String, List[name])] = ${fieldAnns[T, name]("zio.config.magnolia.name")}
  inline def fieldNamesOf[T]: List[(String, List[names])] = ${fieldAnns[T, names]("zio.config.magnolia.names")}
  inline def fieldDocumentationOf[T]: List[(String, List[describe])] = ${fieldAnns[T, describe]("zio.config.magnolia.describe")}

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