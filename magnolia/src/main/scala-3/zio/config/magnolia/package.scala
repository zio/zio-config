package zio.config
package magnolia

import zio.ConfigProvider

import zio.Config
import zio.IO

def deriveConfig[A](using ev: DeriveConfig[A]) =
  ev.desc

type describe = derivation.describe
val describe: derivation.describe.type = derivation.describe

type name = derivation.name
val name: derivation.name.type = derivation.name

type discriminator = derivation.discriminator
val discriminator: derivation.discriminator.type = derivation.discriminator

type kebabCase = derivation.kebabCase
val kebabCase: derivation.kebabCase.type = derivation.kebabCase

type snakeCase = derivation.snakeCase
val snakeCase: derivation.snakeCase.type = derivation.snakeCase

type prefix = derivation.prefix
val prefix: derivation.prefix.type = derivation.prefix

type postfix = derivation.postfix
val postfix: derivation.postfix.type = derivation.postfix

// If you happen to define a Config directly as an implicit, then automatically DeriveConfig will be available
given deriveConfigFromConfig[A](using ev: Config[A]): DeriveConfig[A] =
  DeriveConfig(ev, None)

extension (configProvider: ConfigProvider) {
  def autoLoad[A: DeriveConfig]: IO[Config.Error, A] =
    configProvider.load(DeriveConfig[A].desc)
}
