package zio.config.shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait TypeName[T] {
  def apply(): String
}

object TypeName {
  implicit def materializeTypeName[T]: TypeName[T] = macro typeNameImpl[T]

  def typeNameImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val T    = c.weakTypeOf[T]
    val name = T.typeSymbol.name.toString
    q" new _root_.zio.config.shapeless.TypeName[$T]{ def apply(): String = $name } "
  }
}
