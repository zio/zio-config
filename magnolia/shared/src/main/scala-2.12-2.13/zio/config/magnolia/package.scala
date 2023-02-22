package zio.config

import zio.Config

package object magnolia {
  def deriveConfig[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe

  type name = derivation.name
  val name: derivation.name.type = derivation.name
}
