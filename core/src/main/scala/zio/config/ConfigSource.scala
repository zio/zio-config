package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system
import java.{ util => ju }

/**
 * A config value is a list of A, meaning, multiple A's can exist for a key K.
 * This can happen if the same key occurs in the config-source multiple times and there may or may not be
 * a value of type A associated with it.
 *
 * Another possibility is, the value itself is a list for a single key K.
 *
 * Both these scenarios can be easily represented as ::[Option[A]].
 */
final case class ConfigValue[A](value: ::[Option[A]])

/**
 *
 * ConfigSource represents the source to the config-description, which can be used for reading the values.
 *
 * @param getConfigValue : For a key, ConfigValue may or may not exist.
 *                       Or in the process of fetching it, the computation can fail with some unknown error
 *
 * @param sourceDescription: Every ConfigSource is associated with a description
 */
final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => IO[ReadError.Unknown[Vector[K]], Option[ConfigValue[V]]],
  sourceDescription: List[String]
) { self =>
  final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(
      k =>
        getConfigValue(k).foldM(
          _ => that.getConfigValue(k), {
            case None        => that.getConfigValue(k)
            case a @ Some(_) => ZIO.succeed(a)
          }
        ),
      self.sourceDescription ++ that.sourceDescription
    )

  final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] = self orElse that
}

object ConfigSource {
  val SystemEnvironment = "system environment"
  val SystemProperties  = "system properties"
  val JavaProperties    = "java properties"
  val ConstantMap       = "constant <map>"
  val EmptySource       = "<empty>"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource(
      (k: Vector[K]) => IO.fail(ReadError.Unknown(k, new RuntimeException("Source not provided"))),
      EmptySource :: Nil
    )

  /**
   * ConfigSource representing System environment.
   * @param valueSeparator: The value corresponding to a key will be split by valueSeparator, and will be used to form the list.
   */
  def fromEnv(valueSeparator: Option[String] = None): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString("_")
        system
          .env(key)
          .bimap(
            ReadError.Unknown(path, _),
            opt =>
              opt.map(r => {
                val consOfValues: ::[Option[String]] =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail =>
                            Option(::(Option(h), tail.map(Option(_)))) // Option instead of Some for scala 2.12
                          case Nil => None
                        }
                    )
                    .getOrElse(::(Option(r), Nil))

                ConfigValue(consOfValues)
              })
          )
      },
      SystemEnvironment :: Nil
    )

  /**
   * ConfigSource representing System environment.
   * @param valueSeparator: The value corresponding to a key will be split by valueSeparator, and will be used to form the list.
   */
  def fromProperty(valueSeparator: Option[String] = None): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        system
          .property(key)
          .bimap(
            ReadError.Unknown(path, _),
            opt =>
              opt.map(r => {
                val consOfValues: ::[Option[String]] =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail => Option(::(Option(h), tail.map(Option(_))))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(Option(r), Nil))
                ConfigValue(consOfValues)
              })
          )
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
   * We have the above functionality as  {{{ Config.fromJavaProperties() }}}
   */
  def fromJavaProperties(
    property: ju.Properties,
    valueSeparator: Option[String] = None
  ): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        ZIO
          .effect(
            Option(property.getProperty(key))
          )
          .bimap(
            ReadError.Unknown(path, _),
            opt =>
              opt.map(r => {
                val consOfValues: ::[Option[String]] =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail => Option(::(Option(h), tail.map(Option(_))))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(Option(r), Nil))
                ConfigValue(consOfValues)
              })
          )
      },
      JavaProperties :: Nil
    )

  /**
   * ConfigSource representing a constant Map.
   * @param pathDelimiter : pathDelimiter forms nested value.
   * Example:
   *
   *       "a" : {
   *          "key": "value"
   *         }
   *
   *        in a json,
   *
   *        is {{{ a.key = value }}} in a Map
   */
  def fromMap(map: Map[String, String], pathDelimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(singleton, pathDelimiter)

  def fromMultiMap(map: Map[String, ::[String]], pathDelimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(identity, pathDelimiter)

  private def fromMapA[A, B](map: Map[String, A])(f: A => ::[B], pathDelimiter: String): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(pathDelimiter)
        ZIO
          .succeed(
            map
              .get(key)
              .map(
                r => {
                  val result = f(r).map(Some(_))
                  ConfigValue(
                    ::(
                      result.head,
                      result.tail
                    )
                  )
                }
              )
          )
      },
      ConstantMap :: Nil
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMultiMap(propertyTree.flattenString(delimiter))
}
