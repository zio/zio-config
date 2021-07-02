package zio.config.magnolia

import scala.quoted.*

object Macros:
  inline def nameAnnotations[T]: List[name] = ${anns[T, name]("zio.config.magnolia.name")}
  inline def namesAnnotations[T]: List[names] = ${anns[T, names]("zio.config.magnolia.names")}
  inline def describe[T]: List[describe] = ${anns[T, describe]("zio.config.magnolia.describe")}
  inline def fieldAnnotations[T]: List[(String, List[Any])] = ${fieldAnns[T]}

  def anns[T: Type, A: Type](ownerName: String)(using Quotes): Expr[List[A]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    
    Expr.ofList {
      tpe.typeSymbol.annotations.filter { a => {
        (a.tpe.typeSymbol.maybeOwner.isNoSymbol || a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal") && (
         a.tpe.typeSymbol.fullName == ownerName
        ) 
      }}.map(_.asExpr.asInstanceOf[Expr[A]])
    }  
  }

  def fieldAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    Expr.ofList {
      tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        Expr(field.name) -> field.annotations.filter { a =>
          a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
            a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
        }.map(_.asExpr.asInstanceOf[Expr[Any]])
      }.filter(_._2.nonEmpty).map { (name, anns) => Expr.ofTuple(name, Expr.ofList(anns)) }
    }
end Macros