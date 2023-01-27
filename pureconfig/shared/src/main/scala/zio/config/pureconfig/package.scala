package zio.config

import _root_.pureconfig.ConfigConvert
import zio.config._
import zio.config.typesafe.configValueConfig

package object pureconfig {
  def fromPureconfig[A](implicit cc: ConfigConvert[A]): Config[A] =
    configValueConfig.transformOrFailLeft(cv => cc.from(cv).left.map(e => s"Pureconfig Error: $e"))(cc.to)
}
