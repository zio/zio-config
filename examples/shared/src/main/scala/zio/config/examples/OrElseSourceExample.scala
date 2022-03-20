package zio.config.examples

import zio.config.magnolia.descriptor
import zio.config._, ConfigDescriptor._
import zio.config._, typesafe._
import zio.test.{assertM, Assertion, DefaultRunnableSpec, ZSpec}

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
          ConfigSource
            .fromHoconString(applicationDevYaml)
            .orElse(ConfigSource.fromHoconString(applicationYaml))
      )
    )

  println(orElseSource)

  assert(orElseSource == ApplicationConfig(KafkaClients(List("localhost:9092", "localhost:9094"))))

}
