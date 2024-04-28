package zio.config

import zio.Config

package object magnolia {
  def deriveConfig[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

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
}
