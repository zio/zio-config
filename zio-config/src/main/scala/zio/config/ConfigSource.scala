package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system

final case class ConfigValue[+A](value: A, sourceDescription: String)

final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => IO[ReadErrorsVector[K, V], ConfigValue[V]],
  sourceDescription: List[String]
) { self =>
  final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(
      k => getConfigValue(k).orElse(that.getConfigValue(k)),
      self.sourceDescription ++ that.sourceDescription
    )

  final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] = self orElse that
}

object ConfigSource {
  val SystemEnvironment = "system environment"
  val SystemProperties  = "system properties"
  val ConstantMap       = "constant <map>"
  val EmptySource       = "<empty>"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource((k: Vector[K]) => IO.fail(singleton(ReadError.MissingValue(k))), EmptySource :: Nil)

  val fromEnv: ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString("_")
        system
          .env(key)
          .bimap(
            ReadError.FatalError(path, _),
            opt => opt.map(ConfigValue(_, SystemEnvironment))
          )
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemEnvironment :: Nil
    )

  val fromProperty: ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        system
          .env(key)
          .bimap(ReadError.FatalError(path, _), opt => opt.map(ConfigValue(_, SystemProperties)))
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemProperties :: Nil
    )

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(delimiter)
        ZIO.fromOption(map.get(key).map(ConfigValue(_, ConstantMap))).mapError { _ =>
          singleton(ReadError.MissingValue(Vector(key)))
        }
      },
      ConstantMap :: Nil
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMap(propertyTree.flattenString(delimiter))
}
