package zio.config.typesafe

import zio.Config
import zio.config.magnolia.deriveConfig
import zio.config.read
import zio.test.Assertion.equalTo
import zio.test.{assertZIO, Spec, ZIOSpecDefault}

object TypesafeConfigProviderZIOTest extends ZIOSpecDefault {

  final case class DataBaseConfig(url: String)

  val configDataBaseConfig: Config[DataBaseConfig] = deriveConfig[DataBaseConfig]

  val hocconConfig: String = s"""url = "some_url""""

  override def spec: Spec[Any, Config.Error] =
    suite("TypesafeConfigProviderZIOTest")(
      test("safe read config") {
        val result   = read(configDataBaseConfig from TypesafeConfigProvider.fromHoconStringZIO(hocconConfig))
        val expected = DataBaseConfig("some_url")
        assertZIO(result)(equalTo(expected))
      }
    )
}
