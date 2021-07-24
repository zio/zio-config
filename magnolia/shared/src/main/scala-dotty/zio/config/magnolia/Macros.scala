package zio.config.magnolia

import scala.quoted.*

object Macros:
  inline def nameOf[T]: List[name] = ${anns[T, name]("zio.config.magnolia.name")}
  inline def namesOf[T]: List[names] = ${anns[T, names]("zio.config.magnolia.names")}
  inline def documentationOf[T]: List[describe] = ${anns[T, describe]("zio.config.magnolia.describe")}
  inline def fieldNameOf[T]: List[(String, List[name])] = ${fieldAnns[T, name]("zio.config.magnolia.name")}
  inline def fieldNamesOf[T]: List[(String, List[names])] = ${fieldAnns[T, names]("zio.config.magnolia.names")}
  inline def fieldDocumentationOf[T]: List[(String, List[describe])] = ${fieldAnns[T, describe]("zio.config.magnolia.describe")}
  inline def defaultValuesOf[T]: List[String] = ${ defaultParmasImpl[T] }

  def defaultParmasImpl[T: Type](using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    val tree = TypeTree.of[T].symbol.tree // Separating this leading to error. Dotty is dangerously confusing at times.
//    val sym = TypeTree.of[T].symbol
//    val comp = TypeTree.of[T].symbol.companionClass
//    val names =
//      for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
//      yield p.name

    //names.foreach(name => if name.startsWith("$lessinit$greater$default") then println("ha") else println(name))

//    val body = comp.asInstanceOf[ClassDef].body
//
//    def idents: List[Ref] =
//      for case deff @ DefDef(name, _, _, _) <- body
//      if name.startsWith("$lessinit$greater$default")
//      yield Ref(deff.symbol)

    val names: List[String] = Nil

    //val namesExpr: Expr[List[String]] =
    Expr.ofList(names.map(Expr(_)))

//    val body =
//      comp.tree.asInstanceOf[ClassDef].body
//
//    val idents: List[Ref] =
//      for case deff @ DefDef(name, _, _, _) <- body
//      if name.startsWith("$lessinit$greater$default")
//      yield Ref(deff.symbol)
//
//    val identsExpr: Expr[List[Any]] =
//      Expr.ofList(idents.map(_.asExpr))
//
//    '{ $namesExpr.zip($identsExpr).toMap }

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