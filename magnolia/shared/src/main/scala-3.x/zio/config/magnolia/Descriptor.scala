package zio.config.magnolia

import zio.config._

case class Descriptor[T](desc: ConfigDescriptor[T])

object Descriptor extends App {
  println("is it all working???")
}
