package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system
import java.{ util => ju }

final case class ConfigValue[A](value: ::[Option[A]])

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
  val JavaProperties    = "java properties"
  val ConstantMap       = "constant <map>"
  val EmptySource       = "<empty>"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource((k: Vector[K]) => IO.fail(singleton(ReadError.MissingValue(k))), EmptySource :: Nil)

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
                          case h :: tail => Some(::(Some(h), tail.map(Some(_))))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(Some(r), Nil))

                ConfigValue(consOfValues)
              })
          )
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemEnvironment :: Nil
    )

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
                          case h :: tail => Some(::(Some(h), tail.map(Some(_))))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(Some(r), Nil))
                ConfigValue(consOfValues)
              })
          )
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
                          case h :: tail => Some(::(Some(h), tail.map(Some(_))))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(Some(r), Nil))
                ConfigValue(consOfValues)
              })
          )
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      JavaProperties :: Nil
    )

  def fromMap(map: Map[String, String], pathDelimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(singleton, pathDelimiter)

  def fromMultiMap(map: Map[String, ::[String]], pathDelimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(identity, pathDelimiter)

  private def fromMapA[A, B](map: Map[String, A])(f: A => ::[B], pathDelimiter: String): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(pathDelimiter)
        ZIO
          .fromOption(
            map
              .get(key)
              .map(
                r =>
                  ConfigValue(
                    ::(
                      f(r).map(Some(_)).head,
                      f(r).map(Some(_)).tail
                    )
                  )
              )
          )
          .mapError { _ =>
            singleton(ReadError.MissingValue(Vector(key)))
          }
      },
      ConstantMap :: Nil
    )

  def fromPropertyTree(
    propertyTree: PropertyTree[String, String],
    delimiter: String = "."
  ): ConfigSource[String, String] =
    fromMultiMap(propertyTree.flattenString(delimiter))
}
