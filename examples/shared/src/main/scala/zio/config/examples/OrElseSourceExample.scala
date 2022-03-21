package zio.config.examples

import zio.config.magnolia.descriptor
import zio.config._, ConfigDescriptor._
import zio.config._, typesafe._, yaml._

object OrElseSourceExample extends App {

  val applicationYaml =
    s"""
        kafkaClients: {
          bootstrapServers: [
              "localhost:9092",
              "localhost:9094"
          ]
      
        }
        """

  val applicationDevYaml =
    s"""
        kafkaClients: {
     
     
        }
        """

  case class KafkaClients(bootstrapServers: List[String])
  case class ApplicationConfig(kafkaClients: KafkaClients)

  val orElseSource =
    zio.Runtime.default.unsafeRun(
      read(
        descriptor[ApplicationConfig] from
          ConfigSource.fromHoconString(applicationDevYaml).orElse(ConfigSource.fromHoconString(applicationYaml))
      )
    )

  assert(orElseSource == ApplicationConfig(KafkaClients(List("localhost:9092", "localhost:9094"))))

  val yamlString =
    s"""
       |kafkaClients:
       |  bootstrapServers:
       |    - "localhost:9092"
       |    - "locathost:9094"
       |""".stripMargin

  val applicationYamlSourceReader =
    ConfigSource.fromYamlReader(scala.io.Source.fromFile("/Users/afsalthaj/application.yml").reader)

  val applicationYamlSourceString =
    ConfigSource.fromYamlString(yamlString)

  val expected =
    ApplicationConfig(
      KafkaClients(List("localhost:9092", "localhost:9094"))
    )

  val desc = nested("kafkaClients") {
    nested("bootstrapServers") {
      list(string)
    }
  }

  val result1 = read(desc from applicationYamlSourceReader)
  val result2 = read(desc from applicationYamlSourceString)

  assert(zio.Runtime.default.unsafeRun(result1) == zio.Runtime.default.unsafeRun(result2))

}
