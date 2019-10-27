package zio.config

import zio.config.ReadErrors.ReadError
import zio.system.System
import zio.{ IO, ZIO }

final case class ConfigSource[K, +V](getConfigValue: K => IO[ReadError[K, V], V], sourceDescription: List[String]) {
  def orElse[V1 >: V](that: => ConfigSource[K, V1]): ConfigSource[K, V1] =
    ConfigSource(k => getConfigValue(k).orElse(that.getConfigValue(k)), sourceDescription ++ that.sourceDescription)
}

object ConfigSource {
  def empty[K] =
    ConfigSource(k => IO.fail(ReadError.MissingValue(k)), List.empty)

  val fromEnv: ConfigSource[Vector[String], String] =
    ConfigSource(
          (path: Vector[String]) => {
            val key = path.mkString("_")
            ZIO.accessM[System](_.system.env(key)
              .mapError { err =>
                ReadError.FatalError(key, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_))
              .mapError(_ => ReadError.MissingValue(path))
          })
      )

  val fromProperty: ConfigSource[Vector[String], String] =
    ConfigSource(
          (path: Vector[String]) => {
            val key = path.mkString(".")
            System.Live.system.property(
              .property(key)
              .mapError { err =>
                ReadError.FatalError(key, err): ReadError[String, String]
              }
              .flatMap(IO.fromOption(_))
              .mapError(_ => ReadError.MissingValue(key))
          }))

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    new ConfigSource[String, String] {
      val configSourceService: ConfigSource.Service[String, String] =
        (path: Vector[String]) => {
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
    fromMap(propertyTree.flattenString(delimiter))
}
