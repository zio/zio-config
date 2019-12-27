package zio.config

import zio.{ IO, ZIO }
import zio.system.System.Live.system

final case class ConfigValue[A](value: ::[A], sourceDescription: String)

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

  def fromEnv(separator: Option[String] = None): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString("_")
        system
          .env(key)
          .bimap(
            ReadError.FatalError(path, _),
            opt => opt.map(r => {
              val consOfValues: ::[String] =
                separator.flatMap(sep => r.split(sep).toList match {
                  case h :: tail => Some(::(h, tail))
                  case Nil => None
                }).getOrElse(::(r, Nil))

              ConfigValue(consOfValues, SystemEnvironment)
            })
          )
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemEnvironment :: Nil
    )

  def fromProperty(separator: Option[String] = None): ConfigSource[String, String] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(".")
        system
          .property(key)
          .bimap(ReadError.FatalError(path, _), opt => opt.map(r => {
            val consOfValues: ::[String] =
              separator.flatMap(sep => r.split(sep).toList match {
                case h :: tail => Some(::(h, tail))
                case Nil => None
              }).getOrElse(::(r, Nil))
            ConfigValue(consOfValues, SystemProperties)
          }
          ))
          .flatMap(IO.fromOption(_))
          .mapError(_ => singleton(ReadError.MissingValue(path)))

      },
      SystemProperties :: Nil
    )

  def fromMap(map: Map[String, String], delimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(singleton, delimiter)

  def fromMultiMap(map: Map[String, ::[String]], delimiter: String = "."): ConfigSource[String, String] =
    fromMapA(map)(identity, delimiter)

  private def fromMapA[A, B](map: Map[String, A])(f: A => ::[B],  delimiter: String): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => {
        val key = path.mkString(delimiter)
        ZIO.fromOption(map.get(key).map(r => ConfigValue(f(r), ConstantMap))).mapError { _ =>
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
