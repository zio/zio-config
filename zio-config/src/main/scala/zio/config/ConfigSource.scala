package zio.config

import zio.system.System
import zio.{IO, ZIO}

trait ConfigSource {
  val configSourceService: ConfigSource.Service
}

object ConfigSource {
  trait Service {
    def getConfigValue(path: String): IO[ReadError, PropertyTree]
  }

  val envSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configSourceService: ConfigSource.Service =
          (path: String) =>
            env.system
              .env(path)
              .mapError { err =>
                ReadError.FatalError(path, err)
              }
              .flatMap(IO.fromOption(_).map(t => PropertyTree.Leaf(t)).mapError(_ => ReadError.MissingValue(path)))
      }
    }

  val propSource: ZIO[System, Nothing, ConfigSource] =
    ZIO.access { env =>
      new ConfigSource {
        val configSourceService: ConfigSource.Service =
          (path: String) =>
            env.system
              .property(path)
              .mapError { err =>
                ReadError.FatalError(path, err)
              }
              .flatMap(IO.fromOption(_).map(t => PropertyTree.Leaf(t)).mapError(_ => ReadError.MissingValue(path)))
      }
    }

  def mapSource(map: Map[String, String]): ConfigSource =
    new ConfigSource {
      val configSourceService: ConfigSource.Service =
        (path: String) =>
          ZIO.fromOption(map.get(path)).map(t => PropertyTree.Leaf(t)).mapError { _ =>
            ReadError.MissingValue(path)
          }
    }

}
