package zio.config.examples.autoderivation

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.TypesafeConfigProvider
import zio.{Config, IO}

import examples._

object AutoDerivationCustomKeys extends App with EitherImpureOps {
  final case class MyConfig(accountId: String, awsRegion: String)

  final case class Region(value: String)

  val camelCaseConfig: String =
    """
      |{
      |  awsRegion: us-east
      |  accountId: abcd
      |}
      |""".stripMargin

  // Default behaviour, and hence no mapKey
  val camelCaseResult: IO[Config.Error, MyConfig] =
    read(
      deriveConfig[MyConfig] from (TypesafeConfigProvider.fromHoconString(camelCaseConfig))
    )

  assert(camelCaseResult equalM MyConfig("abcd", "us-east"))

  val kebabCaseConfig: String =
    """
      |{
      |  aws-region: us-east
      |  account-id: abcd
      |}
      |""".stripMargin

  val kebabCaseResult: IO[Config.Error, MyConfig] =
    read(
      deriveConfig[MyConfig].mapKey(toKebabCase) from (TypesafeConfigProvider.fromHoconString(kebabCaseConfig))
    )

  assert(kebabCaseResult equalM MyConfig("abcd", "us-east"))
}
