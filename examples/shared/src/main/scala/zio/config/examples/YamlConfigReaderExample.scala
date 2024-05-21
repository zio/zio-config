package zio.config.examples

import zio._
import zio.config.magnolia.deriveConfig
import zio.config.yaml.YamlConfigProvider

import scala.io.Source

case class KafkaClients(bootstrapServers: List[String], port: Int, region: String)

object KafkaClients {
  implicit val config: Config[KafkaClients] = deriveConfig[KafkaClients]
}

case class ApplicationConfig(kafkaClients: KafkaClients)
object ApplicationConfig {
  implicit val config: Config[ApplicationConfig] = deriveConfig[ApplicationConfig]
}

object YamlConfigReaderExample extends ZIOAppDefault {

  override val bootstrap =
    Runtime.setConfigProvider(
      YamlConfigProvider.fromYamlReader(Source.fromResource("application.yml").reader())
    )

  def run = for {
    config <- ZIO.config(ApplicationConfig.config)
    _      <- ZIO.debug("bootstrapServers: " + config.kafkaClients.bootstrapServers)
    _      <- ZIO.debug("region: " + config.kafkaClients.region)
    _      <- ZIO.debug("port: " + config.kafkaClients.port)
  } yield ()

}
