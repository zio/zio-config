package zio.config.examples.magnolia

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.typesafe.TypesafeConfigSource
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

object ChangeKeys extends App with EitherImpureOps {
  final case class MyConfig(accountId: String, awsRegion: String)

  import zio.config.magnolia.DeriveConfigDescriptor.Descriptor

  final case class Region(value: String)
  Descriptor[String].transform[Region](Region, _.value)

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
      descriptor[MyConfig].mapKey(toKebabCase) from (TypesafeConfigSource.fromHoconString(kebabCaseConfig).loadOrThrow)
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
      descriptor[MyConfig].mapKey(toSnakeCase) from (TypesafeConfigSource.fromHoconString(snakeCaseConfig).loadOrThrow)
    )
}
