package zio.config.examples

import com.typesafe.config._
import zio.config.typesafe._
import zio.{ IO }
import zio.config._

case class ServiceConfigurationLoader[A](
  schema: zio.config.ConfigDescriptor[A]
) extends ZioConfigExtension {

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
                      "SERVICENAME_SCHEMAREGISTRYURL" -> "schemaregistry:system_env"
                    )
                  )
                )
      ressConf <- configFromResources
      rule: Map[String => String, ConfigSource] = {
        Map[String => String, ConfigSource](
          ((r: String) => r)                                     -> cmdConf,
          ((r: String) => r.toLowerCase())                       -> cmdConf,
          ((r: String) => addPrefixToKey(prefix)(r).toUpperCase) -> sysConf,
          ((r: String) => r.toLowerCase())                       -> sysConf,
          ((r: String) => r)                                     -> ressConf,
          ((r: String) => r.toLowerCase)                         -> ressConf
        )
      }

      updatedSchema = configSchema.updateSourceForEachKey(rule)

    } yield updatedSchema

  private def configFromResources: IO[ReadError[String], ConfigSource] =
    IO.fromEither(TypesafeConfigSource.fromTypesafeConfig(ConfigFactory.defaultApplication()))
}

object ServiceConfigurationLoader {
  val CommandLineKeyDelimiter = '.'
  val EnvVarKeyDelimiter      = '_'
  val EnvVarValueDelimiter    = ','
}

trait ZioConfigExtension {
  implicit class SourceConfigOps[A](config: ConfigDescriptor[A]) {
    import ConfigDescriptorAdt._

    def updateSourceForEachKey(f: Map[String => String, ConfigSource]) =
      f.foldRight(config) { (b, a) =>
        def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] =
          config match {
            case Lazy(thunk)                             => Lazy(() => loop(thunk()))
            case existing @ Source(source, propertyType) => Source(b._2, propertyType)
            case DynamicMap(source, conf)                => DynamicMap(source, loop(conf))
            case exiting @ Nested(source, path, conf) =>
              exiting orElse Nested(source, b._1(path), loop(conf)) orElse Nested(b._2, b._1(path), loop(conf))
            case Optional(conf)          => Optional(loop(conf))
            case Sequence(source, conf)  => Sequence(source, loop(conf))
            case Describe(conf, message) => Describe(loop(conf), message)
            case Default(conf, value)    => Default(loop(conf), value)
            case TransformOrFail(conf, f, g) =>
              TransformOrFail(loop(conf), f, g)
            case Zip(conf1, conf2) => Zip(loop(conf1), loop(conf2))
            case OrElseEither(conf1, conf2) =>
              OrElseEither(loop(conf1), loop(conf2))
            case OrElse(value1, value2) =>
              OrElse(loop(value1), loop(value2))
          }

        loop(a)
      }
  }
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
  performCleanUp: String
)

object Run {
  import zio.config.magnolia.DeriveConfigDescriptor.descriptor
  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default
    val r =
      ServiceConfigurationLoader(descriptor[KafkaConfig])
        .loadConfiguration("serviceName_", List("--bootstrapservers", "bootstrap:commandline"))

    println(runtime.unsafeRun(r))
    // KafkaConfig(bootstrap:commandline,this_is_from_system_env,from hocon source)

  }
}
