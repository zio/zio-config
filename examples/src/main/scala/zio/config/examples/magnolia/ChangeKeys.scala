package zio.config.examples.magnolia

import java.util.Properties

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.typesafe.TypesafeConfigSource
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

object ChangeKeys extends App with EitherImpureOps {
  final case class MyConfig(accountId: String, awsRegion: String)

  val camelCaseConfig =
    """
      |{
      |  awsRegion: us-east
      |  accountId: abcd
      |}
      |""".stripMargin

  // Default behaviour, and hence no mapKey
  val camelCaseResult =
    read(
      descriptor[MyConfig] from (TypesafeConfigSource.fromHoconString(camelCaseConfig).loadOrThrow)
    )

  assert(camelCaseResult == Right(MyConfig("abcd", "us-east")))

  val kebabCaseConfig =
    """
      |{
      |  aws-region: us-east
      |  account-id: abcd
      |}
      |""".stripMargin

  val kebabCaseResult =
    read(
      descriptor[MyConfig].mapKey(camelToKebab) from (TypesafeConfigSource.fromHoconString(kebabCaseConfig).loadOrThrow)
    )

  assert(kebabCaseResult == Right(MyConfig("abcd", "us-east")))

  val snakeCaseConfig =
    """
      |{
      |  aws_region: us-east
      |  account_id: abcd
      |}
      |""".stripMargin

  val snakeCaseResult =
    read(
      descriptor[MyConfig].mapKey(camelToSnake) from (TypesafeConfigSource.fromHoconString(snakeCaseConfig).loadOrThrow)
    )
}

abstract class ZIOConfigApp[Config](
  val appName: String
)(implicit config: zio.config.magnolia.DeriveConfigDescriptor.Descriptor[Config]) {
  val appConfig = read(config from ConfigSource.fromProperties({
    val prop = new Properties()
    prop.put("s", "vaalue")
    prop
  }))
}

object Hello extends App {
  case class MyConf(s: String)

  println(new ZIOConfigApp[MyConf]("appname") {}.appConfig)
}
