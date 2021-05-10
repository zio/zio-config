package zio.config.examples.magnolia

import com.github.ghik.silencer.silent
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfigSource

@silent("deprecated")
object AutoDerivationCustomKeys extends App with EitherImpureOps {
  final case class MyConfig(accountId: String, awsRegion: String)

  import zio.config.magnolia.DeriveConfigDescriptor.Descriptor

  final case class Region(value: String)
  Descriptor[String].transform[Region](Region, _.value)

  val camelCaseConfig: String =
    """
      |{
      |  awsRegion: us-east
      |  accountId: abcd
      |}
      |""".stripMargin

  // Default behaviour, and hence no mapKey
  val camelCaseResult: Either[ReadError[String], MyConfig] =
    read(
      descriptor[MyConfig] from (TypesafeConfigSource.fromHoconString(camelCaseConfig).loadOrThrow)
    )

  assert(camelCaseResult == Right(MyConfig("abcd", "us-east")))

  val kebabCaseConfig: String =
    """
      |{
      |  aws-region: us-east
      |  account-id: abcd
      |}
      |""".stripMargin

  val kebabCaseResult: Either[ReadError[String], MyConfig] =
    read(
      descriptor[MyConfig].mapKey(toKebabCase) from (TypesafeConfigSource.fromHoconString(kebabCaseConfig).loadOrThrow)
    )

  assert(kebabCaseResult == Right(MyConfig("abcd", "us-east")))

  val snakeCaseConfig: String =
    """
      |{
      |  aws_region: us-east
      |  account_id: abcd
      |}
      |""".stripMargin

  val snakeCaseResult: Either[ReadError[String], MyConfig] =
    read(
      descriptor[MyConfig].mapKey(toSnakeCase) from (TypesafeConfigSource.fromHoconString(snakeCaseConfig).loadOrThrow)
    )
}
