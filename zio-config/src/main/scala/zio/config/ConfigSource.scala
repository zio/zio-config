package zio.config

import zio.config.ReadErrors.ReadError
import zio.system.System
import zio.{ IO, ZIO }

trait ConfigSource[K, V] {
  val configSourceService: ConfigSource.Service[K, V]
}

object ConfigSource {
  trait Service[K, V] {
    def getConfigValue(path: List[K]): IO[ReadError[K, V], V]
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
