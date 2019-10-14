package zio.config

import zio.config.ReadErrors.ReadError
import zio.system.System
import zio.{ IO, ZIO }

trait ConfigSource[K, V] { self =>
  val configService: ConfigSource.Service[K, V]

  final def orElse(that: ConfigSource): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        new ConfigSource.Service {
          val sourceDescription: String = {
            val selfDesc = self.configService.sourceDescription
            val thatDesc = that.configService.sourceDescription

            s"${selfDesc}: with fallback source: ${thatDesc} (on all errors)"
          }

          def getConfigValue(path: String): IO[ReadError, ConfigSource.Value] =
            self.configService
              .getConfigValue(path)
              .orElse(
                that.configService
                  .getConfigValue(path)
              )
        }
    }

  final def <>(that: ConfigSource): ConfigSource =
    orElse(that)

  final def catchSome(
    pf: PartialFunction[ReadError, ConfigSource],
    fallbackSource: String,
    fallbackDescription: String
  ): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        new ConfigSource.Service {
          val sourceDescription: String = {
            val selfDesc = self.configService.sourceDescription

            s"${selfDesc}: with fallback source: ${fallbackSource} (${fallbackDescription})"
          }

          def getConfigValue(path: String): IO[ReadError, ConfigSource.Value] =
            self.configService
              .getConfigValue(path)
              .catchSome {
                pf.andThen(
                  _.configService
                    .getConfigValue(path)
                )
              }
        }
    }

  final def ifMissingValue(that: => ConfigSource): ConfigSource =
    catchSome(
      { case ReadError.MissingValue(_) => that },
      that.configService.sourceDescription,
      "on missing value"
    )

  final def <>?(that: ConfigSource): ConfigSource =
    ifMissingValue(that)
}

object ConfigSource {
  final case class Value(value: String, valueSource: String)

  trait Service[K, V] {
    val sourceDescription: String

    def getConfigValue(path: List[K]): IO[ReadError[K, V], Value]
  }

  val fromEnv: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          (path: List[String]) => {
            val key = path.mkString("_")
            env.system
              .env(key)
              .mapError { err =>
                ReadError.FatalError(key, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_))
              .mapError(_ => ReadError.MissingValue(key))
          }
      }
    }

  val fromProperty: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          (path: List[String]) => {
            val key = path.mkString(".")
            env.system
              .property(key)
              .mapError { err =>
                ReadError.FatalError(key, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_))
              .mapError(_ => ReadError.MissingValue(key))
          }
      }
    }

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    new ConfigSource[String, String] {
      val configSourceService: ConfigSource.Service[String, String] =
        (path: List[String]) => {
          val key = path.mkString(delimiter)
          ZIO.fromOption(map.get(key)).mapError { _ =>
            ReadError.MissingValue(key): ReadError[String, String]
          }
        }
    }

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMap(propertyTree.flatten(delimiter))
}
