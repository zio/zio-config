package zio.config

import zio.ConfigProvider

import zio.Config
import zio.IO

package object magnolia {
  def deriveConfig[A](implicit ev: DeriveConfig[A]) =
    ev.desc

  // If you happen to define a Config directly as an implicit, then automatically DeriveConfig will be available
  implicit def deriveConfigFromConfig[A](implicit ev: Config[A]): DeriveConfig[A] =
    DeriveConfig(ev, None)

  implicit class ConfigProviderOps[A](configProvider: ConfigProvider) {
    def autoLoad[A: DeriveConfig]: IO[Config.Error, A] =
      configProvider.load(DeriveConfig[A])
  }
}
