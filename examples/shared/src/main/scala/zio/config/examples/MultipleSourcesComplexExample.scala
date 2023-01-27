package zio.config.examples

import com.typesafe.config._
import zio._
import zio.config._
import zio.config.magnolia.descriptor
import zio.config.typesafe.TypesafeConfigSource

object ConfigLoader {
  def apply[A](
    prefix: String,
    args: List[String],
    schema: Config[A]
  ): IO[Config.Error, A] =
    for {
      config        <- getConfigProgram(prefix, args, schema)
      serviceConfig <- read(config)
    } yield serviceConfig

  private def getConfigProgram[A](
    prefix: String,
    args: List[String],
    configSchema: Config[A]
  ): IO[Config.Error, Config[A]] =
    for {
      cmdConf <- ZIO.succeed(
                   ConfigSource.fromCommandLineArgs(args, Some('.'))
                 )
      // for demonstration: this should be ur sysEnv
      sysConf <- ZIO.succeed(
                   ConfigProvider.fromMap(
                     Map(
                       // Prefix added only for system env
                       "SERVICENAME_SCHEMAREGISTRYURL" -> "schemaregistry:system_env",
                       "SERVICENAME_serialization"     -> "serdes:system_env"
                     )
                   )
                 )

      // application.conf in resource folder
      ressConf = TypesafeConfigSource.fromTypesafeConfig(ZIO.attempt(ConfigFactory.defaultApplication()))

      sourceSpec = cmdConf <>
                     cmdConf.mapKeys(_.toLowerCase()) <>
                     sysConf.mapKeys(key => addPrefixToKey(prefix.toUpperCase())(key.toLowerCase())) <>
                     sysConf.mapKeys(r => addPrefixToKey(prefix)(r).toUpperCase) <>
                     ressConf <>
                     ressConf.mapKeys(_.toLowerCase())

      updatedSchema = configSchema.updateSource(_ => sourceSpec)

    } yield updatedSchema
}

object KafkaApplication {
  final case class KafkaConfig(
    bootstrapServers: String,
    schemaRegistryUrl: String,
    serialization: String,
    performCleanUp: String
  )
}

object MultipleSourcesComplexExample extends zio.ZIOAppDefault {
  def run: URIO[Any, ExitCode] = {
    val pgm =
      ConfigLoader(
        "serviceName_",
        List("--bootstrapServers", "bootstrap:commandline"),
        descriptor[KafkaApplication.KafkaConfig]
      )

    pgm.flatMap(r => Console.printLine(r.toString)).exitCode
    // KafkaConfig(bootstrap:commandline,schemaregistry:system_env,from hocon source)
  }
}
