package zio.config.examples

import zio.config._
import zio.config.magnolia.descriptor
import zio.config.typesafe._
import zio.config.yaml._

object ConfigSourceOrElseExample extends App {

  val applicationYaml: String =
    s"""
        kafkaClients: {
          bootstrapServers: [
              "localhost:9092",
              "localhost:9094"
          ],
          port: 100

        }
        """

  val applicationDevYaml: String =
    s"""
        kafkaClients: {
          region: US
        }
        """

  case class KafkaClients(bootstrapServers: List[String], port: Int, region: String)
  case class ApplicationConfig(kafkaClients: KafkaClients)

  val orElseSource: ApplicationConfig =
    zio.Runtime.default.unsafeRun(
      read(
        descriptor[ApplicationConfig] from
          ConfigSource.fromHoconString(applicationDevYaml).orElse(ConfigSource.fromHoconString(applicationYaml))
      )
    )

  // region US is picked from first source
  // port 100 is picked from second source
  // list is picked from second source
  assert(orElseSource == ApplicationConfig(KafkaClients(List("localhost:9092", "localhost:9094"), 100, "US")))

  val yamlString: String =
    s"""
       |kafkaClients:
       |  bootstrapServers:
       |    - "localhost:9092"
       |    - "locathost:9094"
       |  port: "100"
       |  region: "US"
       |""".stripMargin

  val applicationYamlSourceReader: ConfigSource =
    ConfigSource.fromYamlReader(scala.io.Source.fromResource("application.yml").reader())

  val applicationYamlSourceString: ConfigSource =
    ConfigSource.fromYamlString(yamlString)

  val expected: ApplicationConfig =
    ApplicationConfig(KafkaClients(List("localhost:9092", "locathost:9094"), 100, "US"))

  val desc: ConfigDescriptor[ApplicationConfig] = descriptor[ApplicationConfig]

  val result1: ApplicationConfig = zio.Runtime.default.unsafeRun(read(desc from applicationYamlSourceReader))
  val result2: ApplicationConfig = zio.Runtime.default.unsafeRun(read(desc from applicationYamlSourceString))

  assert(result1 == expected)
  assert(result1 == result2)

}
