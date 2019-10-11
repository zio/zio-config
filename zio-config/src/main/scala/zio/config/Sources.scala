package zio.config

import zio.system.System
import zio.{ IO, ZIO }

trait Sources {
  val envSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          new ConfigSource.Service {
            val sourceDescription: String = "Environment"

            def getConfigValue(path: String): IO[ReadError, String] =
              env.system
                .env(path)
                .mapError { err =>
                  ReadError.FatalError(path, err)
                }
                .flatMap(IO.fromOption(_).mapError(_ => ReadError.MissingValue(path)))
          }
      }
    }

  val propSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          new ConfigSource.Service {
            val sourceDescription: String = "System properties"

            def getConfigValue(path: String): IO[ReadError, String] =
              env.system
                .property(path)
                .mapError { err =>
                  ReadError.FatalError(path, err)
                }
                .flatMap(IO.fromOption(_).mapError(_ => ReadError.MissingValue(path)))
          }
      }
    }

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        new ConfigSource.Service {
          val sourceDescription: String = "Scala Map"

          def getConfigValue(path: String): IO[ReadError, String] =
            ZIO.fromOption(map.get(path)).mapError { _ =>
              ReadError.MissingValue(path)
            }
        }
    }

  // TODO HOCON etc as separate modules
}
