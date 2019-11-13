package zio.config

import zio.config.ReadErrors.ReadError
import zio.system.System
import zio.{ IO, ZIO }
import zio.system.System.Live.system

final case class ConfigSource[K, +V](
  getConfigValue: K => IO[ReadError[K, V], V], sourceDescription: List[String]
) {
  def orElse[V1 >: V](that: => ConfigSource[K, V1]): ConfigSource[K, V1] =
    ConfigSource(k => getConfigValue(k).orElse(that.getConfigValue(k)), sourceDescription ++ that.sourceDescription)
}

object ConfigSource {
  def empty[K]: ConfigSource[Vector[K], Nothing] = {
    ConfigSource((k: Vector[K]) => IO.fail(ReadError.MissingValue(k)), List.empty)
  }

  val fromEnv: ConfigSource[Vector[String], String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString("_")
        system
          .env(key)
          .mapError { err =>
            ReadError.FatalError[Vector[String]](path, err)
          }
          .flatMap(IO.fromOption(_))
          .mapError(_ => ReadError.MissingValue(path))

      },
      List("system environment")
    )

  val fromProperty: ConfigSource[Vector[String], String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        system
          .env(key)
          .mapError { err =>
            ReadError.FatalError(path, err)
          }
          .flatMap(IO.fromOption(_))
          .mapError(_ => ReadError.MissingValue(path))

      },
      List("system properties")
    )

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[Vector[String], String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(delimiter)
        ZIO.fromOption(map.get(key)).mapError { _ =>
          ReadError.MissingValue(Vector(key))
        }
      },
      List("constant <map>")
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[Vector[String], String] =
    fromMap(propertyTree.flattenString(delimiter))
}
