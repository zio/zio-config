package zio.config.examples

import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import zio.ConfigProvider

import java.io.InputStreamReader

object ConfigSourceOrElseExample extends App {

  val applicationHocon: String =
    s"""
        kafkaClients: {
          bootstrapServers: [
              "localhost:9092",
              "localhost:9094"
          ],
          port: 100

        }
        """

  val applicationDevHocon: String =
    s"""
        kafkaClients: {
          region: US
        }
        """

  case class KafkaClients(bootstrapServers: List[String], port: Int, region: String)

  case class ApplicationConfig(kafkaClients: KafkaClients)

  val orElseSource: ApplicationConfig =
    Unsafe.unsafe { implicit u =>
      zio.Runtime.default.unsafe
        .run(
          read(
            deriveConfig[ApplicationConfig] from
              ConfigProvider
                .fromHoconString(applicationDevHocon)
                .orElse_(ConfigProvider.fromHoconString(applicationHocon))
          )
        )
        .getOrThrowFiberFailure()
    }

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

  val applicationYamlSourceReader: ConfigProvider =
    ConfigProvider.fromYamlReader(scala.io.Source.fromResource("application.yml").reader())

  val applicationYamlSourceString: ConfigProvider =
    ConfigProvider.fromYamlString(yamlString)

  val expected: ApplicationConfig =
    ApplicationConfig(KafkaClients(List("localhost:9092", "locathost:9094"), 100, "US"))

  val desc: Config[ApplicationConfig] = deriveConfig[ApplicationConfig]

  val result1: ApplicationConfig =
    Unsafe.unsafe { implicit u =>
      zio.Runtime.default.unsafe.run(read(desc from applicationYamlSourceReader)).getOrThrowFiberFailure()
    }
  val result2: ApplicationConfig =
    Unsafe.unsafe { implicit u =>
      zio.Runtime.default.unsafe.run(read(desc from applicationYamlSourceString)).getOrThrowFiberFailure()
    }

  assert(result1 == expected)
  assert(result1 == result2)

}
