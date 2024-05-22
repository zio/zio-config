package zio.config.examples.configsources

import zio.config._
import zio.config.yaml._
import zio.{Config, _}

object XmlConfigReaderExample extends ZIOAppDefault {

  final case class Aws(region: String, account: String)

  object Aws {
    val config: Config[Aws] = Config.string("region").zip(Config.string("account")).to[Aws]
  }

  final case class Database(port: Int, url: String)

  object Database {
    val config: Config[Database] = Config.int("port").zip(Config.string("url")).to[Database]
  }

  final case class Configuration(aws: Aws, database: Database)

  object Configuration {
    val config: Config[Configuration] =
      Aws.config.nested("aws").zip(Database.config.nested("database")).to[Configuration].nested("config")
  }

  override val bootstrap =
    Runtime.setConfigProvider(
      ConfigProvider.fromYamlReader(scala.io.Source.fromResource("application.xml").reader())
    )

  def run =
    for {
      config <- ZIO.config(Configuration.config)
      _      <- ZIO.debug("aws region: " + config.aws.region)
      _      <- ZIO.debug("aws account: " + config.aws.account)
      _      <- ZIO.debug("db port: " + config.database.port)
      _      <- ZIO.debug("db url: " + config.database.url)
    } yield ()

}
