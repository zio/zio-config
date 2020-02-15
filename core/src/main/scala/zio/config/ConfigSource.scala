package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system
import java.{ util => ju }

final case class ConfigValue[A](value: ::[Option[::[A]]])

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
                val consOfValues =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail =>
                            Option(::(h, tail)) // Option instead of Some for scala 2.12
                          case Nil => None
                        }
                    )
                    .getOrElse(::(r, Nil))

                ConfigValue(singleton(Some(consOfValues)))
              })
          )
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
                val consOfValues =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail => Option(::(h, tail))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(r, Nil))
                ConfigValue(singleton(Some(consOfValues)))
              })
          )
      },
      SystemProperties :: Nil
    )

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
                val consOfValues =
                  valueSeparator
                    .flatMap(
                      sep =>
                        r.split(sep).toList.map(_.trim) match {
                          case h :: tail => Option(::(h, tail))
                          case Nil       => None
                        }
                    )
                    .getOrElse(::(r, Nil))
                ConfigValue(singleton(Some(consOfValues)))
              })
          )
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
          .succeed(
            map
              .get(key)
              .map(
                r => {
                  val result = f(r)
                  ConfigValue(
                    singleton(
                      Some(
                        ::(
                          result.head,
                          result.tail
                        )
                      )
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
