package zio.config.examples

import com.typesafe.config._
import zio.{ IO }

import zio.config._, typesafe._
case class ServiceConfigurationLoader[A](
  schema: zio.config.ConfigDescriptor[A]
) {

  def loadConfiguration(
    prefix: String,
    args: List[String]
  ): IO[ReadError[String], A] =
    for {
      config        <- getConfigProgram(prefix, args, schema)
      serviceConfig <- IO.fromEither(read(config))
    } yield serviceConfig

  private def getConfigProgram[A](
    prefix: String,
    args: List[String],
    configSchema: ConfigDescriptor[A]
  ): IO[ReadError[String], ConfigDescriptor[A]] =
    for {
      cmdConf <- IO.succeed(
                  ConfigSource.fromCommandLineArgs(args, Some(ServiceConfigurationLoader.CommandLineKeyDelimiter))
                )
      // for demonstration: this should be ur sysEnv
      sysConf <- IO.succeed(
                  ConfigSource.fromMap(
                    Map(
                      // Prefix added only for system env
                      "SERVICENAME_SCHEMAREGISTRYURL" -> "schemaregistry:system_env",
                      "SERVICENAME_serialization"     -> "serdes:system_env"
                    )
                  )
                )

      // application.conf in resource folder
      ressConf <- IO.fromEither(TypesafeConfigSource.fromTypesafeConfig(ConfigFactory.defaultApplication()))

      sourceSpec = cmdConf <>
        cmdConf.convertKeys(_.toLowerCase()) <>
        sysConf.convertKeys(key => addPrefixToKey(prefix.toUpperCase())(key.toLowerCase())) <>
        sysConf.convertKeys(r => addPrefixToKey(prefix)(r).toUpperCase) <>
        ressConf <>
        ressConf.convertKeys(_.toLowerCase())

      updatedSchema = configSchema.updateSource(_ => sourceSpec)

    } yield updatedSchema

}

object ServiceConfigurationLoader {
  val CommandLineKeyDelimiter = '.'
  val EnvVarKeyDelimiter      = '_'
  val EnvVarValueDelimiter    = ','
}

/**
 *  Base parameter trait to extends by every services.
 */
trait ServiceParameters {
  val serviceName: String
  val kafka: KafkaConfig
}
case class KafkaConfig(
  bootstrapServers: String,
  schemaRegistryUrl: String,
  serialization: String,
  performCleanUp: String
)

object KeySourcePriorityExample {
  import zio.config.magnolia.DeriveConfigDescriptor.descriptor

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default
    val r =
      ServiceConfigurationLoader(descriptor[KafkaConfig])
        .loadConfiguration("serviceName_", List("--bootstrapServers", "bootstrap:commandline"))

    println(runtime.unsafeRun(r))
    // KafkaConfig(bootstrap:commandline,schemaregistry:system_env,from hocon source)

  }
}
