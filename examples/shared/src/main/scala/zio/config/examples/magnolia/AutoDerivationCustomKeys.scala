package zio.config.examples.magnolia

import com.github.ghik.silencer.silent
import zio.IO
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.deriveConfig
import zio.Config
import zio.config.typesafe.TypesafeConfigSource

import examples._

@silent("deprecated")
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
      deriveConfig[MyConfig] from (TypesafeConfigSource.fromHoconString(camelCaseConfig))
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
      deriveConfig[MyConfig].mapKey(toKebabCase) from (TypesafeConfigSource.fromHoconString(kebabCaseConfig))
    )

  assert(kebabCaseResult equalM MyConfig("abcd", "us-east"))
}
