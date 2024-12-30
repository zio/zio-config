package zio.config

import zio.{Config, ConfigProvider}
import zio.IO

import scala.deriving.Mirror

package object magnolia {
  def deriveConfig[A](implicit ev: DeriveConfig[A]) =
    ev.desc

  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe

  type name = derivation.name
  val name: derivation.name.type = derivation.name

  type discriminator = derivation.discriminator
  val discriminator: derivation.discriminator.type = derivation.discriminator

  // If you happen to define a Config directly as an implicit, then automatically DeriveConfig will be available
  implicit def deriveConfigFromConfig[A](implicit ev: Config[A]): DeriveConfig[A] =
    DeriveConfig(ev, None)

  implicit class ConfigProviderOps[A](configProvider: ConfigProvider) {
    def autoLoad[A: DeriveConfig]: IO[Config.Error, A] =
      configProvider.load(DeriveConfig[A].desc)
  }

  /**
   * Enables derivation of Config via the `derives Config` syntax
   */
  extension (? : Config.type) {
    inline def derived[A](using Mirror.Of[A]): Config[A] = DeriveConfig.derived[A].desc
  }

}
