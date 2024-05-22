package zio.config.examples.configsources

import zio._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.FromConfigSourceTypesafe

object TypesafeHoconStringSourceExample extends ZIOAppDefault {

  case class SimpleConfig(port: Int, url: String, region: Option[String])

  object SimpleConfig {
    val config: Config[SimpleConfig] = deriveConfig[SimpleConfig]
  }

  override val bootstrap =
    Runtime.setConfigProvider(
      ConfigProvider
        .fromHoconString(
          """
            |{
            |  port: 123
            |  url: bla
            |  region: useast
            |}
            |""".stripMargin
        )
        .orElse(
          ConfigProvider.fromHoconString(
            """
              |port=123
              |url=bla
              |region=useast
              |""".stripMargin
          )
        )
    )

  def run =
    for {
      config <- ZIO.config(SimpleConfig.config)
      _      <- ZIO.debug("port: " + config.port)
      _      <- ZIO.debug("url: " + config.url)
      _      <- ZIO.debug("region: " + config.region)
    } yield ()
}
