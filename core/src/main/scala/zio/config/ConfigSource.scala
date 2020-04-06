package zio.config

import java.{ util => ju }

import zio.UIO

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil
import zio.config.PropertyTree.{ unflatten, Leaf, Record, Sequence }
import zio.ZIO
import java.io.FileInputStream
import java.io.File
import These._
import zio.Task

final case class ConfigSource[K, V](
  getConfigValue: Vector[K] => PropertyTree[K, V],
  sourceDescription: List[String]
) { self =>
  final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    ConfigSource(
      k => getConfigValue(k).getOrElse(that.getConfigValue(k)),
      if (self.sourceDescription == that.sourceDescription) self.sourceDescription
      else self.sourceDescription ++ that.sourceDescription
    )

  final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] =
    self orElse that
}

object ConfigSource {
  private[config] val SystemEnvironment    = "system environment"
  private[config] val SystemProperties     = "system properties"
  private[config] val CommandLineArguments = "command line arguments"

  def empty[K, V]: ConfigSource[K, V] =
    ConfigSource(_ => PropertyTree.empty, Nil)

  /**
   * EXPERIMENTAL
   *
   * Forming configuration from command line arguments.
   *
   * Assumption. All keys should start with either - or --
   *
   * This source supports almost all standard command-line patterns including nesting/sub-config, repetition/list etc
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *    args = "-db.username=1 --db.password=hi --vault -username=3 --vault -password=10 --regions 111,122 --user k1 --user k2"
   *    keyDelimiter   = Some('.')
   *    valueDelimiter = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *
   *    final case class Credentials(username: String, password: String)
   *
   *    val credentials = (string("username") |@| string("password"))(Credentials.apply, Credentials.unapply)
   *
   *    final case class Config(databaseCredentials: Credentials, vaultCredentials: Credentials, regions: List[String, users: List[String])
   *
   *    (nested("db") { credentials } |@| nested("vault") { credentials } |@| list(string("regions") |@| list(string("user"))(Config.apply, Config.unapply)
   *
   *    // res0 Config(Credentials(1, hi), Credentials(3, 10), List(111, 122), List(k1, k2))
   *
   * }}}
   *
   * @see [[https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples/commandline/CommandLineArgsExample.scala]]
   */
  def fromCommandLineArgs(
    args: List[String],
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  ): ConfigSource[String, String] =
    ConfigSource.fromPropertyTrees(
      getPropertyTreeFromArgs(args.filter(_.nonEmpty), keyDelimiter, valueDelimiter),
      CommandLineArguments
    )

  /**
   * Provide keyDelimiter if you need to consider flattened config as a nested config.
   * Provide valueDelimiter if you need any value to be a list
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *    map            = Map("KAFKA_SERVERS" -> "server1, server2", "KAFKA_SERDE"  -> "confluent")
   *    keyDelimiter   = Some('_')
   *    valueDelimiter = Some(',')
   * }}}
   *
   * tthen, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromMap(
    map: Map[String, String],
    source: String = "constant",
    keyDelimiter: Option[Char] = None,
    valueDelimter: Option[Char] = None
  ): ConfigSource[String, String] =
    fromMapInternal(map)(
      x => {
        val listOfValues = valueDelimter.fold(List(x))(delim => x.split(delim).toList)
        ::(listOfValues.head, listOfValues.tail)
      },
      keyDelimiter,
      source
    )

  /**
   * Provide keyDelimiter if you need to consider flattened config as a nested config.
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *   map = Map("KAFKA_SERVERS" -> singleton(server1), "KAFKA_SERDE"  -> singleton("confluent"))
   *   keyDelimiter = Some('_')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromMultiMap(
    map: Map[String, ::[String]],
    source: String = "constant",
    keyDelimiter: Option[Char] = None
  ): ConfigSource[String, String] =
    fromMapInternal(map)(identity, keyDelimiter, source)

  /**
   * Provide keyDelimiter if you need to consider flattened config as a nested config.
   * Provide valueDelimiter if you need any value to be a list
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *   property      = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
   *   keyDelimiter   = Some('.')
   *   valueDelimiter = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromProperties(
    property: ju.Properties,
    source: String = "properties",
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  ): ConfigSource[String, String] = {
    val mapString = property.stringPropertyNames().asScala.foldLeft(Map.empty[String, String]) { (acc, a) =>
      acc.updated(a, property.getProperty(a))
    }

    mergeAll(
      PropertyTree
        .fromStringMap(mapString, keyDelimiter, valueDelimiter)
        .map(tree => fromPropertyTree(tree, source))
    )
  }

  /**
   * Provide keyDelimiter if you need to consider flattened config as a nested config.
   * Provide valueDelimiter if you need any value to be a list
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *   properties (in file) = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
   *   keyDelimiter         = Some('.')
   *   valueDelimiter       = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromPropertiesFile[A](
    filePath: String,
    keyDelimiter: Option[Char] = None,
    valueDelimiter: Option[Char] = None
  ): Task[ConfigSource[String, String]] =
    for {
      properties <- ZIO.bracket(ZIO.effect(new FileInputStream(new File(filePath))))(r => ZIO.effectTotal(r.close()))(
                     inputStream => {
                       ZIO.effect {
                         val properties = new java.util.Properties()
                         properties.load(inputStream)
                         properties
                       }
                     }
                   )
    } yield ConfigSource.fromProperties(properties, filePath, keyDelimiter, valueDelimiter)

  def fromSystemEnv: UIO[ConfigSource[String, String]] =
    fromSystemEnv(None, None)

  /**
   * Consider providing keyDelimiter if you need to consider flattened config as a nested config.
   * Consider providing valueDelimiter if you need any value to be a list
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *    vars in sys.env  = "KAFKA_SERVERS" = "server1, server2" ; "KAFKA_SERDE" = "confluent"
   *    keyDelimiter     = Some('_')
   *    valueDelimiter   = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   *
   * Note: The delimiter '.' for keys doesn't work in system environment.
   */
  def fromSystemEnv(keyDelimiter: Option[Char], valueDelimiter: Option[Char]): UIO[ConfigSource[String, String]] =
    UIO
      .effectTotal(sys.env)
      .map(map => ConfigSource.fromMap(map, SystemEnvironment, keyDelimiter, valueDelimiter))

  def fromSystemProperties: UIO[ConfigSource[String, String]] =
    fromSystemProperties(None, None)

  /**
   * Consider providing keyDelimiter if you need to consider flattened config as a nested config.
   * Consider providing valueDelimiter if you need any value to be a list
   *
   * Example:
   *
   * Given:
   *
   * {{{
   *    vars in sys.env  = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
   *    keyDelimiter     = Some('.')
   *    valueDelimiter   = Some(',')
   * }}}
   *
   * then, the following works:
   *
   * {{{
   *    final case class kafkaConfig(server: String, serde: String)
   *    nested("KAFKA")(string("SERVER") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
   * }}}
   */
  def fromSystemProperties(
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): UIO[ConfigSource[String, String]] =
    for {
      systemProperties <- UIO.effectTotal(java.lang.System.getProperties)
    } yield ConfigSource.fromProperties(
      property = systemProperties,
      source = SystemProperties,
      keyDelimiter = keyDelimiter,
      valueDelimiter = valueDelimiter
    )

  private[config] def fromMapInternal[A, B](
    map: Map[String, A]
  )(f: A => ::[B], keyDelimiter: Option[Char], source: String): ConfigSource[String, B] =
    fromPropertyTrees(
      unflatten(
        map.map(
          tuple => {
            val vectorOfKeys = keyDelimiter match {
              case Some(keyDelimiter) => tuple._1.split(keyDelimiter).toVector.filterNot(_.trim == "")
              case None               => Vector(tuple._1)
            }
            vectorOfKeys -> f(tuple._2)
          }
        )
      ),
      source
    )

  private[config] def fromPropertyTree[B](
    tree: PropertyTree[String, B],
    source: String
  ): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => tree.getPath(path.toList),
      source :: Nil
    )

  private[config] def fromPropertyTrees[B](
    trees: Iterable[PropertyTree[String, B]],
    sourceName: String
  ): ConfigSource[String, B] =
    mergeAll(trees.map(fromPropertyTree(_, sourceName)))

  /// CommandLine Argument Source

  private[config] final case class Value(value: String) extends AnyVal

  type KeyValue = These[Key, Value]

  private[config] object KeyValue {
    def mk(s: String): Option[KeyValue] = {
      val splitted = s.split('=').toList

      (splitted.headOption, splitted.lift(1)) match {
        case (Some(possibleKey), Some(possibleValue)) =>
          Key.mk(possibleKey) match {
            case Some(actualKey) => Some(Both(actualKey, Value(possibleValue)))
            case None            => Some(That(Value(possibleValue)))
          }
        case (None, Some(possibleValue)) =>
          Some(That(Value(possibleValue)))

        case (Some(possibleKey), None) =>
          Key.mk(possibleKey) match {
            case Some(value) => Some(This(value))
            case None        => Some(That(Value(possibleKey)))
          }

        case (None, None) => None
      }
    }
  }

  private[config] class Key private (val value: String) extends AnyVal {
    override def toString: String = value
  }

  object Key {
    def mk(s: String): Option[Key] =
      if (s.startsWith("--")) {
        Some(new Key(s.replace("--", "")))
      } else if (s.startsWith("-")) {
        Some(new Key(s.replace("-", "")))
      } else
        None
  }

  private[config] def getPropertyTreeFromArgs(
    args: List[String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): List[PropertyTree[String, String]] = {
    def unFlattenWith(
      key: String,
      tree: PropertyTree[String, String]
    ): PropertyTree[String, String] =
      keyDelimiter.fold(Record(Map(key -> tree)): PropertyTree[String, String])(
        value => unflatten(key.split(value).toList, tree)
      )

    def toSeq[V](leaf: String): PropertyTree[String, String] =
      valueDelimiter.fold(Sequence(List(Leaf(leaf))): PropertyTree[String, String])(
        c => Sequence[String, String](leaf.split(c).toList.map(Leaf(_)))
      )

    def loop(args: List[String]): List[PropertyTree[String, String]] =
      args match {
        case h1 :: h2 :: h3 =>
          (KeyValue.mk(h1), KeyValue.mk(h2)) match {
            case (Some(keyValue1), Some(keyValue2)) =>
              (keyValue1, keyValue2) match {
                case (Both(l1, r1), Both(l2, r2)) =>
                  unFlattenWith(l1.value, toSeq(r1.value)) ::
                    unFlattenWith(l2.value, toSeq(r2.value)) :: loop(h3)

                case (Both(l1, r1), This(l2)) =>
                  unFlattenWith(l1.value, toSeq(r1.value)) :: h3.headOption.fold(
                    List.empty[PropertyTree[String, String]]
                  )(
                    x =>
                      loop(List(x)).map(
                        tree => unFlattenWith(l2.value, tree)
                      ) ++ loop(h3.tail)
                  )

                case (Both(l1, r1), That(r2)) =>
                  unFlattenWith(l1.value, toSeq(r1.value)) :: toSeq(r2.value) :: loop(h3)

                case (This(l1), Both(l2, r2)) =>
                  unFlattenWith(l1.value, unFlattenWith(l2.value, toSeq(r2.value))) :: loop(h3)

                case (This(l1), This(l2)) =>
                  val keysAndTrees =
                    h3.zipWithIndex.map {
                      case (key, index) => (index, loop(List(key)))
                    }.find(_._2.nonEmpty)

                  keysAndTrees match {
                    case Some((index, trees)) =>
                      val keys = seqOption(h3.take(index).map(Key.mk))

                      keys.fold(List.empty[PropertyTree[String, String]]) { nestedKeys =>
                        trees
                          .map(tree => unflatten(l2.value :: nestedKeys.map(_.value), tree))
                          .map(tree => unFlattenWith(l1.value, tree)) ++ loop(h3.drop(index + 1))
                      }

                    case None => Nil
                  }

                case (This(l1), That(r2)) =>
                  unFlattenWith(l1.value, toSeq(r2.value)) :: loop(h3)

                case (That(r1), Both(l2, r2)) =>
                  toSeq(r1.value) :: unFlattenWith(l2.value, toSeq(r2.value)) :: loop(h3)

                case (That(r1), That(r2)) =>
                  toSeq(r1.value) :: toSeq(r2.value) :: loop(h3)

                case (That(r1), This(l2)) =>
                  toSeq(r1.value) :: loop(h3).map(tree => unFlattenWith(l2.value, tree))
              }

            case (Some(_), None) =>
              loop(h1 :: h3)
            case (None, Some(_)) =>
              loop(h2 :: h3)
            case (None, None) =>
              loop(h3)
          }

        case h1 :: Nil =>
          KeyValue.mk(h1) match {
            case Some(value) =>
              value.fold(
                (left, right) => unFlattenWith(left.value, toSeq(right.value)) :: Nil,
                _ => Nil, // This is an early Nil unlike others.
                value => toSeq(value.value) :: Nil
              )

            case None => Nil
          }
        case Nil => Nil
      }

    PropertyTree.mergeAll(loop(args))
  }

  private[config] def mergeAll[K, V](sources: Iterable[ConfigSource[K, V]]): ConfigSource[K, V] =
    sources.foldLeft(ConfigSource.empty: ConfigSource[K, V])(_ orElse _)

}
