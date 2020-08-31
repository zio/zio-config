package zio.config

import java.{ util => ju }

import zio.{ IO, Task, UIO, ZIO }
import zio.system.System

import scala.collection.JavaConverters._
import scala.collection.immutable.Nil
import zio.config.PropertyTree.{ unflatten, Leaf, Record, Sequence }
import java.io.FileInputStream
import java.io.File

import These._

trait ConfigSourceModule extends KeyValueModule {

  case class ConfigSourceName(name: String)

  trait ConfigSource { self =>
    def names: Set[ConfigSourceName]
    def getConfigValue: List[K] => PropertyTree[K, V]
    def leafForSequence: LeafForSequence

    def orElse(that: => ConfigSource): ConfigSource =
      getConfigSource(
        self.names ++ that.names,
        path => self.getConfigValue(path).getOrElse(that.getConfigValue(path)),
        that.leafForSequence
      )

    def <>(that: => ConfigSource): ConfigSource = self orElse that
  }

  protected def getConfigSource(
    sourceNames: Set[ConfigSourceName],
    getTree: List[K] => PropertyTree[K, V],
    isLeafValidSequence: LeafForSequence
  ): ConfigSource =
    new ConfigSource { self =>
      def names: Set[ConfigSourceName]                  = sourceNames
      def getConfigValue: List[K] => PropertyTree[K, V] = getTree
      def leafForSequence: LeafForSequence              = isLeafValidSequence
    }

  /**
   * To specify if a singleton leaf should be considered
   * as a valid sequence or not.
   */
  sealed trait LeafForSequence

  object LeafForSequence {
    case object Invalid extends LeafForSequence
    case object Valid   extends LeafForSequence
  }

  trait ConfigSourceFunctions {
    val empty: ConfigSource =
      getConfigSource(Set.empty, _ => PropertyTree.empty, LeafForSequence.Valid)

    protected def dropEmpty(tree: PropertyTree[K, V]): PropertyTree[K, V] =
      if (tree.isEmpty) PropertyTree.Empty
      else
        tree match {
          case l @ Leaf(_) => l
          case Record(value) =>
            Record(value.filterNot { case (_, v) => v.isEmpty })
          case PropertyTree.Empty => PropertyTree.Empty
          case Sequence(value)    => Sequence(value.filterNot(_.isEmpty))
        }

    protected def dropEmpty(
      trees: List[PropertyTree[K, V]]
    ): List[PropertyTree[K, V]] = {
      val res = trees.map(dropEmpty(_)).filterNot(_.isEmpty)
      if (res.isEmpty) PropertyTree.Empty :: Nil
      else res
    }

    protected def unwrapSingletonLists(
      tree: PropertyTree[K, V]
    ): PropertyTree[K, V] = tree match {
      case l @ Leaf(_) => l
      case Record(value) =>
        Record(value.map { case (k, v) => k -> unwrapSingletonLists(v) })
      case PropertyTree.Empty     => PropertyTree.Empty
      case Sequence(value :: Nil) => unwrapSingletonLists(value)
      case Sequence(value)        => Sequence(value.map(unwrapSingletonLists(_)))
    }

    protected def unwrapSingletonLists(
      trees: List[PropertyTree[K, V]]
    ): List[PropertyTree[K, V]] =
      trees.map(unwrapSingletonLists(_))

    /**
     *
     * To obtain a config source directly from a property tree.
     *
     * @param tree            : PropertyTree
     * @param source          : Label the source with a name
     * @param leafForSequence : Should a single value wrapped in Leaf be considered as Sequence
     * @return
     */
    def fromPropertyTree(
      tree: PropertyTree[K, V],
      source: String,
      leafForSequence: LeafForSequence
    ): ConfigSource =
      getConfigSource(Set(ConfigSourceName(source)), tree.getPath, leafForSequence)

    protected def fromPropertyTrees(
      trees: Iterable[PropertyTree[K, V]],
      source: String,
      leafForSequence: LeafForSequence
    ): ConfigSource =
      mergeAll(trees.map(fromPropertyTree(_, source, leafForSequence)))

    private[config] def mergeAll(
      sources: Iterable[ConfigSource]
    ): ConfigSource =
      sources.reduceLeftOption(_ orElse _).getOrElse(empty)
  }

  protected object ConfigSourceFunctions extends ConfigSourceFunctions
}

trait ConfigSourceStringModule extends ConfigSourceModule {
  type K = String
  type V = String

  object ConfigSource extends ConfigSourceFunctions {
    private[config] val SystemEnvironment    = "system environment"
    private[config] val SystemProperties     = "system properties"
    private[config] val CommandLineArguments = "command line arguments"

    /**
     * EXPERIMENTAL

     * Assumption. All keys should start with -
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
     *    final case class Config(databaseCredentials: Credentials, vaultCredentials: Credentials, regions: List[String], users: List[String])
     *
     *    (nested("db") { credentials } |@| nested("vault") { credentials } |@| list("regions")(string) |@| list("user")(string))(Config.apply, Config.unapply)
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
    ): ConfigSource =
      ConfigSource.fromPropertyTrees(
        getPropertyTreeFromArgs(
          args.filter(_.nonEmpty),
          keyDelimiter,
          valueDelimiter
        ),
        CommandLineArguments,
        LeafForSequence.Valid
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
     * then, the following works:
     *
     * {{{
     *    final case class kafkaConfig(server: String, serde: String)
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * leafForSequence indicates whether a Leaf(value) (i.e, a singleton) could be considered a Sequence.
     */
    def fromMap(
      constantMap: Map[String, String],
      source: String = "constant",
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): ConfigSource =
      fromMapInternal(constantMap)(
        x => {
          val listOfValues = {
            valueDelimiter.fold(List(x))(delim => x.split(delim).toList.map(_.trim))
          }

          ::(listOfValues.head, listOfValues.tail)
        },
        keyDelimiter,
        ConfigSourceName(source),
        leafForSequence
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
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * leafForSequence indicates whether a Leaf(value) (i.e, a singleton) could be considered a Sequence.
     */
    def fromMultiMap(
      map: Map[String, ::[String]],
      source: String = "constant",
      keyDelimiter: Option[Char] = None,
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): ConfigSource =
      fromMapInternal(map)(identity, keyDelimiter, ConfigSourceName(source), leafForSequence)

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
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * leafForSequence indicates whether a Leaf(value) (i.e, a singleton) could be considered a Sequence.
     */
    def fromProperties(
      property: ju.Properties,
      source: String = "properties",
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): ConfigSource = {
      val mapString = property
        .stringPropertyNames()
        .asScala
        .foldLeft(Map.empty[String, String]) { (acc, a) =>
          acc.updated(a, property.getProperty(a))
        }

      mergeAll(
        unwrapSingletonLists(
          dropEmpty(
            PropertyTree.fromStringMap(mapString, keyDelimiter, valueDelimiter)
          )
        ).map(tree => fromPropertyTree(tree, source, leafForSequence))
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
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * leafForSequence indicates whether a Leaf(value) (i.e, a singleton) could be considered a Sequence.
     */
    def fromPropertiesFile[A](
      filePath: String,
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): Task[ConfigSource] =
      for {
        properties <- ZIO.bracket(
                       ZIO.effect(new FileInputStream(new File(filePath)))
                     )(r => ZIO.effectTotal(r.close()))(inputStream => {
                       ZIO.effect {
                         val properties = new java.util.Properties()
                         properties.load(inputStream)
                         properties
                       }
                     })
      } yield ConfigSource.fromProperties(
        properties,
        filePath,
        keyDelimiter,
        valueDelimiter,
        leafForSequence
      )

    def fromSystemEnv: ZIO[System, ReadError[String], ConfigSource] =
      fromSystemEnv(None, None)

    /**
     *
     * For users that dont want to use layers in their application
     * This method provides live system environment layer
     *
     */
    def fromSystemEnvLive(
      keyDelimiter: Option[Char],
      valueDelimiter: Option[Char],
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): IO[ReadError[String], ConfigSource] =
      fromSystemEnv(keyDelimiter, valueDelimiter, leafForSequence).provideLayer(System.live)

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
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     *
     * Note: The delimiter '.' for keys doesn't work in system environment.
     */
    def fromSystemEnv(
      keyDelimiter: Option[Char],
      valueDelimiter: Option[Char],
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): ZIO[System, ReadError[String], ConfigSource] = {
      val validDelimiters = ('a' to 'z') ++ ('A' to 'Z') :+ '_'

      if (keyDelimiter.forall(validDelimiters.contains)) {
        ZIO
          .accessM[System](_.get.envs)
          .bimap(
            error =>
              ReadError.SourceError[String](s"Error while getting system environment variables: ${error.getMessage}"),
            fromMap(_, SystemEnvironment, keyDelimiter, valueDelimiter, leafForSequence)
          )
      } else {
        IO.fail(ReadError.SourceError[String](s"Invalid system key delimiter: ${keyDelimiter.get}"))
      }
    }

    def fromSystemProperties: UIO[ConfigSource] =
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
     *    nested("KAFKA")(string("SERVERS") |@| string("SERDE"))(KafkaConfig.apply, KafkaConfig.unapply)
     * }}}
     */
    def fromSystemProperties(
      keyDelimiter: Option[Char],
      valueDelimiter: Option[Char],
      leafForSequence: LeafForSequence = LeafForSequence.Valid
    ): UIO[ConfigSource] =
      for {
        systemProperties <- UIO.effectTotal(java.lang.System.getProperties)
      } yield ConfigSource.fromProperties(
        property = systemProperties,
        source = SystemProperties,
        keyDelimiter = keyDelimiter,
        valueDelimiter = valueDelimiter,
        leafForSequence = leafForSequence
      )

    private def fromMapInternal[A](map: Map[String, A])(
      f: A => ::[String],
      keyDelimiter: Option[Char],
      source: ConfigSourceName,
      leafForSequence: LeafForSequence
    ): ConfigSource =
      fromPropertyTrees(
        unwrapSingletonLists(dropEmpty(unflatten(map.map(tuple => {
          val vectorOfKeys = keyDelimiter match {
            case Some(keyDelimiter) =>
              tuple._1.split(keyDelimiter).toVector.filterNot(_.trim == "")
            case None => Vector(tuple._1)
          }
          vectorOfKeys -> f(tuple._2)
        })))),
        source.name,
        leafForSequence
      )

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
        valueDelimiter.fold(
          Sequence(List(Leaf(leaf))): PropertyTree[String, String]
        )(c => Sequence[String, String](leaf.split(c).toList.map(Leaf(_))))

      /// CommandLine Argument Source

      case class Value(value: String)

      type KeyValue = These[Key, Value]

      object KeyValue {
        def mk(s: String): Option[KeyValue] =
          splitAtFirstOccurence(s, "=") match {
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

        def splitAtFirstOccurence(text: String, char: String): (Option[String], Option[String]) = {
          val splitted = text.split(char, 2)
          splitted.headOption.filterNot(_.isEmpty) -> splitted.lift(1)
        }
      }

      class Key private (val value: String) {
        override def toString: String = value
      }

      object Key {
        def mk(s: String): Option[Key] =
          if (s.startsWith("-")) {
            val key = removeLeading(s, '-')
            if (key.nonEmpty) Some(new Key(key)) else None
          } else {
            None
          }

        def removeLeading(s: String, toRemove: Char): String =
          s.headOption match {
            case Some(c) if c == toRemove => removeLeading(s.tail, toRemove)
            case _                        => s
          }
      }

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
                    unFlattenWith(l1.value, toSeq(r1.value)) :: h3.headOption
                      .fold(List.empty[PropertyTree[String, String]])(
                        x =>
                          loop(List(x))
                            .map(tree => unFlattenWith(l2.value, tree)) ++ loop(
                            h3.tail
                          )
                      )

                  case (Both(l1, r1), That(r2)) =>
                    unFlattenWith(l1.value, toSeq(r1.value)) :: toSeq(r2.value) :: loop(
                      h3
                    )

                  case (This(l1), Both(l2, r2)) =>
                    unFlattenWith(
                      l1.value,
                      unFlattenWith(l2.value, toSeq(r2.value))
                    ) :: loop(h3)

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
                            .map(
                              tree =>
                                unflatten(
                                  l2.value :: nestedKeys.map(_.value),
                                  tree
                                )
                            )
                            .map(tree => unFlattenWith(l1.value, tree)) ++ loop(
                            h3.drop(index + 1)
                          )
                        }

                      case None => Nil
                    }

                  case (This(l1), That(r2)) =>
                    unFlattenWith(l1.value, toSeq(r2.value)) :: loop(h3)

                  case (That(r1), Both(l2, r2)) =>
                    toSeq(r1.value) :: unFlattenWith(l2.value, toSeq(r2.value)) :: loop(
                      h3
                    )

                  case (That(r1), That(r2)) =>
                    toSeq(r1.value) :: toSeq(r2.value) :: loop(h3)

                  case (That(r1), This(l2)) =>
                    toSeq(r1.value) :: loop(h3).map(
                      tree => unFlattenWith(l2.value, tree)
                    )
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

      unwrapSingletonLists(dropEmpty(PropertyTree.mergeAll(loop(args))))
    }
  }
}
