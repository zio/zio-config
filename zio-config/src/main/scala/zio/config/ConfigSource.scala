package zio.config

import zio.system.System
import zio.{ IO, ZIO }

trait ConfigSource[K, V] {
  val configSourceService: ConfigSource.Service[K, V]
}

object ConfigSource {
  trait Service[K, V] {
    def getConfigValue(path: K): IO[ReadError[K, V], PropertyTree[K, V]]
  }

  val envSource: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          (path: String) =>
            env.system
              .env(path)
              .mapError { err =>
                ReadError.FatalError(path, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_).map(t => PropertyTree.Leaf(t)).mapError(_ => ReadError.MissingValue(path)))
      }
    }

  val propSource: ZIO[System, Nothing, ConfigSource[String, String]] =
    ZIO.access { env =>
      new ConfigSource[String, String] {
        val configSourceService: ConfigSource.Service[String, String] =
          (path: String) =>
            env.system
              .property(path)
              .mapError { err =>
                ReadError.FatalError(path, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_).map(t => PropertyTree.Leaf(t)).mapError(_ => ReadError.MissingValue(path)))
      }
    }

  def mapSource[K, V](map: Map[K, V]): ConfigSource[K, V] =
    new ConfigSource[K, V] {
      val configSourceService: ConfigSource.Service[K, V] =
        (path: K) =>
          ZIO.fromOption(map.get(path)).map(t => PropertyTree.Leaf(t): PropertyTree[K, V]).mapError { _ =>
            ReadError.MissingValue(path): ReadError[K, V]
          }
    }

}
