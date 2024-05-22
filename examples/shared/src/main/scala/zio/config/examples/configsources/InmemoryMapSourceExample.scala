package zio.config.examples.configsources

import zio._
import zio.config.magnolia.deriveConfig

object InmemoryMapSourceExample extends ZIOAppDefault {

  case class KafkaClients(bootstrapServers: List[String], port: Int, region: String)

  object KafkaClients {
    implicit val config: Config[KafkaClients] = deriveConfig[KafkaClients]
  }

  case class ApplicationConfig(kafkaClients: KafkaClients)
  object ApplicationConfig {
    implicit val config: Config[ApplicationConfig] = deriveConfig[ApplicationConfig]
  }

  override val bootstrap =
    Runtime.setConfigProvider(
      ConfigProvider.fromMap(
        Map(
          "kafkaClients.bootstrapServers[0]" -> "foo",
          "kafkaClients.bootstrapServers[1]" -> "bar",
          "kafkaClients.port"                -> "9092",
          "kafkaClients.region"              -> "EU"
        )
      )
    )

  def run = for {
    config <- ZIO.config(ApplicationConfig.config)
    _      <- ZIO.debug("bootstrapServers: " + config.kafkaClients.bootstrapServers)
    _      <- ZIO.debug("region: " + config.kafkaClients.region)
    _      <- ZIO.debug("port: " + config.kafkaClients.port)
  } yield ()

}
