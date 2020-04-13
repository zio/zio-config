package zio.config.magnolia

import magnolia.Magnolia

import scala.reflect.macros.whitebox

object DescriptorMacro {

  def gen[T: c.WeakTypeTag](c: whitebox.Context)(ev: c.Tree): c.Tree = {
    val _ = ev
    Magnolia.gen[T](c)
  }
}
