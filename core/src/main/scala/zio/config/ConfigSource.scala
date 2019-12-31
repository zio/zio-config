package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system
import java.{ util => ju }

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
  val JavaProperties    = "java properties"

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
          .property(key)
          .bimap(ReadError.FatalError(path, _), opt => opt.map(ConfigValue(_, SystemProperties)))
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemProperties :: Nil
    )

  /**
   * Pass any Java Properties that you have and you get a ConfigSource.
   * zio-config tries to not make assumptions on the placement of property file. It may exist
   * in classpath, or it could be in cloud (AWS S3). Loading to properties file is user's
   * responsiblity to make things flexible. A typical usage will be
   *  {{{
   *      ZIO
   *        .bracket(ZIO.effect(new FileInputStream("file location")))(file => ZIO.effectTotal(file.close()))(
   *         file =>
   *            ZIO.effect {
   *              val properties = new java.util.Properties()
   *              properties.load(file)
   *              properties
   *            }).map(r => fromJavaProperties(r))
   *  }}}
   */
  def fromJavaProperties(
    property: ju.Properties,
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        ZIO
          .effect(
            Option(property.getProperty(key))
          )
          .bimap(ReadError.FatalError(path, _), opt => opt.map(ConfigValue(_, JavaProperties)))
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      JavaProperties :: Nil
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
