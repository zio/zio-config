package zio.config

import _root_.pureconfig.ConfigConvert
import zio.config._
import zio.config.typesafe.configValueConfigDescriptor

package object pureconfig {
  def fromPureconfig[A](implicit cc: ConfigConvert[A]): ConfigDescriptor[A] =
    configValueConfigDescriptor.transformOrFailLeft(cv => cc.from(cv).left.map(e => s"Pureconfig Error: $e"))(cc.to)
}
