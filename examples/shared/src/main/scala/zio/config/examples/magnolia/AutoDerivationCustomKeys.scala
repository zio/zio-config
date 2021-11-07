package zio.config.examples.magnolia

import com.github.ghik.silencer.silent
import zio.config._, examples._
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
  val camelCaseResult =
    read(
      descriptor[MyConfig] from (TypesafeConfigSource.fromHoconString(camelCaseConfig))
    )

  assert(camelCaseResult equalM MyConfig("abcd", "us-east"))

  val kebabCaseConfig: String =
    """
      |{
      |  aws-region: us-east
      |  account-id: abcd
      |}
      |""".stripMargin

  val kebabCaseResult =
    read(
      descriptor[MyConfig].mapKey(toKebabCase) from (TypesafeConfigSource.fromHoconString(kebabCaseConfig))
    )

  assert(kebabCaseResult equalM MyConfig("abcd", "us-east"))
}
