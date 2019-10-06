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
              .flatMap { (oStr: Option[String]) =>
                oStr.fold[IO[ReadError, String]](
                  ZIO.fail(ReadError.MissingValue(path, "Not defined in system environment"))
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
              .flatMap { (oStr: Option[String]) =>
                oStr.fold[IO[ReadError, String]](
                  ZIO.fail(ReadError.MissingValue(path, "System property not found for key"))
                )(ZIO.succeed)
              }
      }
    }

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configService: ConfigSource.Service =
        (path: String) =>
          ZIO.fromOption(map.get(path)).mapError { _ =>
            ReadError.MissingValue(path, "Key not in map")
          }
    }

  // TODO HOCON etc as separate modules
}
