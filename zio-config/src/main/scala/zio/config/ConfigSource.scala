package zio.config

import zio.config.ReadErrors.ReadError
import zio.{ IO, ZIO }
import zio.system.System.Live.system

final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => IO[ConfigErrors[K, V], V],
  sourceDescription: List[String]
) {
  def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(k => getConfigValue(k).orElse(that.getConfigValue(k)), sourceDescription ++ that.sourceDescription)

  def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    orElse(that)

}

object ConfigSource {
  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource((k: Vector[K]) => IO.fail(ReadErrors(ReadError.MissingValue(k))), List.empty)

  val fromEnv: ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString("_")
        system
          .env(key)
          .mapError { err =>
            ReadError.FatalError[Vector[String], String](path, err)
          }
          .flatMap(IO.fromOption(_))
          .mapError(_ => ReadErrors(ReadError.MissingValue[Vector[String], String](path)))

      },
      List("system environment")
    )

  val fromProperty: ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        system
          .env(key)
          .mapError { err =>
            ReadError.FatalError(path, err)
          }
          .flatMap(IO.fromOption(_))
          .mapError(_ => ReadErrors(ReadError.MissingValue[Vector[String], String](path)))

      },
      List("system properties")
    )

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(delimiter)
        ZIO.fromOption(map.get(key)).mapError { _ =>
          ReadErrors(ReadError.MissingValue[Vector[String], String](Vector(key)))
        }
      },
      List("constant <map>")
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMap(propertyTree.flattenString(delimiter))
}
