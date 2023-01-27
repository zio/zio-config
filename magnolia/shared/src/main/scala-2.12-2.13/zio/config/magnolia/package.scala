package zio.config

import zio.Config

package object magnolia {
  def deriveConfig[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

}
