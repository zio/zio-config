package zio.config

import zio.system.System
import zio.{ IO, ZIO }

trait Sources {
  val envSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            env.system
              .env(path)
              .mapError { err =>
                ReadError.FatalError(path, err)
              }
              .flatMap {
                _.fold[IO[ReadError, String]](
                  ZIO.fail(ReadError.MissingValue(path))
                )(ZIO.succeed)
              }
      }
    }

  val propSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configService: ConfigSource.Service =
          (path: String) =>
            env.system
              .property(path)
              .mapError { err =>
                ReadError.FatalError(path, err)
              }
              .flatMap {
                _.fold[IO[ReadError, String]](
                  ZIO.fail(ReadError.MissingValue(path))
                )(ZIO.succeed)
              }
      }
    }

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        (path: String) =>
          ZIO.fromOption(map.get(path)).mapError { _ =>
            ReadError.MissingValue(path)
          }
    }

  // TODO HOCON etc as separate modules
}
