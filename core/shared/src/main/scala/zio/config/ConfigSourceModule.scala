package zio.config

import com.github.ghik.silencer.silent
import zio.{Has, IO, System, UIO, ZIO, ZLayer, ZManaged}

import java.io.{File, FileInputStream}
import java.{util => ju}
import scala.collection.immutable.Nil
import scala.jdk.CollectionConverters._

import PropertyTree.{Leaf, Record, Sequence, unflatten}

trait ConfigSourceModule extends KeyValueModule {
  // Currently all sources are just String and String
  type K = String
  type V = String

  import ConfigSource._

  sealed trait ConfigSource { self =>

    /**
     * With `strictlyOnce`, regardless of the number of times `read`
     * is invoked, `ConfigSource` is evaluated
     * strictly once.
     *
     * It returns an Effect, because by the time ConfigSource is retrieved,
     * an effect is performed (which may involve a resource acquisition and release)
     *
     * {{{
     *   val sourceZIO = ConfigSource.fromPropertiesFile(...).strictlyOnce
     *
     *   for {
     *     src     <- sourceZIO
     *     result1 <- read(config from src)
     *     result2 <- read(config from src)
     *   } yield (result1, result2)
     *
     * }}}
     *
     * In this case, the propertiesFile is read only once.
     *
     * vs
     *
     * {{{
     *   val source: ConfigSource =
     *     ConfigSource.fromPropertiesFile(...).memoize
     *
     *   for {
     *     result1 <- read(config from source)
     *     result2 <- read(config from source)
     *   } yield (result1, result2)
     *
     * }}}
     *
     * In this case, the propertiesFile is read once per each read, i.e, twice.
     */
    @silent("a type was inferred to be `Any`")
    def strictlyOnce: ZIO[Any, ReadError[K], ConfigSource] =
      (self match {
        case ConfigSource.OrElse(self, that) =>
          self.strictlyOnce.orElse(that.strictlyOnce)

        case ConfigSource.Reader(names, access) =>
          val strictAccess = access.flatMap(identity).use(value => ZIO.succeed(value))
          strictAccess.map(reader => Reader(names, ZManaged.succeed(ZManaged.succeed(reader))))
      })

    /**
     * A Layer is assumed to be "memoized" by default, i.e the construction
     * of ConfigSource layer is done strictly once regardless of number times the read is invoked.
     */
    def toLayer: ZLayer[Any, ReadError[K], Has[ConfigSource]] =
      strictlyOnce.toLayer

    /**
     * Transform keys before getting queried from source. Note that, this method could be hardly useful.
     * Most of the time all you need to use is `mapKeys` in `ConfigDescriptor`
     * i.e, `read(descriptor[Config].mapKeys(f) from ConfigSource.fromMap(source))`
     *
     * If you are still curious to understand `mapKeys` in `ConfigSource`, then read on, or else
     * avoid a confusion.
     *
     * {{{
     *   case class Hello(a: String, b: String)
     *   val config: ConfigDescriptor[Hello] = (string("a") |@| string("b")).to[Hello]
     *
     *   However your source is different for some reason (i.e, its not `a` and `b`). Example:
     *   {
     *     "aws_a" : "1"
     *     "aws_b" : "2"
     *   }
     *
     *   If you are not interested in changing the `descriptor` or `case class`, you have a freedom
     *   to pre-map keys before its queried from ConfigSource
     *
     *   val removeAwsPrefix  = (s: String) = s.replace("aws", "")
     *
     *   val source = ConfigSource.fromMap(map)
     *   val updatedSource = source.mapKeys(removeAwsPrefix)
     *
     *   read(config from updatedSource)
     *
     *   // This is exactly the same as
     *
     *   val addAwsPrefix = (s: String) = s"aws_${s}")
     *   read(config.mapKeys(addAwsPrefix) from source)
     * }}}
     */
    def mapKeys(f: K => K): ConfigSource =
      self match {
        case ConfigSource.OrElse(left, right) =>
          ConfigSource.OrElse(left.mapKeys(f), right.mapKeys(f))

        case reader @ ConfigSource.Reader(_, _) =>
          reader.copy(access = reader.access.map(_.map(fn => (path: PropertyTreePath[K]) => fn(path.mapKeys(f)))))
      }

    def run: Reader =
      self match {
        case OrElse(self, that)    => self.run.orElse(that.run)
        case reader @ Reader(_, _) => reader
      }

    /**
     * Within a `read`, ConfigSource is evaluated only once if memoized.
     *
     * Example:
     *   {{{
     *     (string("x") |@| string("y")).to[Config] from databaseSource
     *   }}}
     *
     * In the above case, within a single database connection, `x` and `y` is
     * retrieved
     *
     * However, for every individual read,
     * ConfigSource will be re-computed.
     *
     * If ConfigSource need to be computed only once even for
     * multiple reads, then consider using `strictlyOnce` combinator
     * or use `toLayer`
     */
    def memoize: ConfigSource =
      self match {
        case OrElse(self, that)    =>
          self.memoize.orElse(that.memoize)
        case reader @ Reader(_, _) =>
          reader.copy(access = reader.access.flatMap(_.memoize))
      }

    def sourceNames: Set[ConfigSource.ConfigSourceName] =
      self match {
        case OrElse(self, that)     => self.sourceNames ++ that.sourceNames
        case Reader(sourceNames, _) => sourceNames
      }

    def orElse(that: ConfigSource): ConfigSource =
      OrElse(self, that)

    def <>(that: ConfigSource): ConfigSource =
      orElse(that)

    def runTree(path: PropertyTreePath[K]): IO[ReadError[K], PropertyTree[K, V]] =
      self match {
        case OrElse(self, that) =>
          self.runTree(path).orElse(that.runTree(path))

        case Reader(_, access) =>
          access.use(_.use(tree => tree(path)))
      }

    def at(propertyTreePath: PropertyTreePath[K]): ConfigSource = self match {
      case OrElse(self, that)    => self.at(propertyTreePath).orElse(that.at(propertyTreePath))
      case Reader(names, access) =>
        Reader(names, access.map(_.map(fn => (path => fn(propertyTreePath).map(_.at(path))))))
    }
  }

  object ConfigSource {
    type Managed[A]              = ZManaged[Any, ReadError[K], A]
    type TreeReader              = PropertyTreePath[K] => ZIO[Any, ReadError[K], PropertyTree[K, V]]
    type MemoizableManaged[A]    = ZManaged[Any, Nothing, ZManaged[Any, ReadError[K], A]]
    type ManagedReader           = Managed[TreeReader]
    type MemoizableManagedReader = MemoizableManaged[TreeReader]

    case class ConfigSourceName(name: String)

    private[config] val SystemEnvironment    = "system environment"
    private[config] val SystemProperties     = "system properties"
    private[config] val CommandLineArguments = "command line arguments"

    val empty: ConfigSource =
      Reader(
        Set.empty,
        ZManaged.succeed(ZManaged.succeed(_ => ZIO.succeed(PropertyTree.empty)))
      )

    case class OrElse(self: ConfigSource, that: ConfigSource) extends ConfigSource

    case class Reader(
      names: Set[ConfigSource.ConfigSourceName],
      access: ConfigSource.MemoizableManagedReader
    ) extends ConfigSource { self =>

      /**
       * Try `this` (`configSource`), and if it fails, try `that` (`configSource`)
       *
       * For example:
       *
       * Given three configSources, `configSource1`, `configSource2` and `configSource3`, such that
       * configSource1 and configSource2 will only have `id` and `configSource3` act as a global fall-back source.
       *
       * The following config tries to fetch `Id` from configSource1, and if fails, it tries `configSource2`,
       * and if both fails it gets from `configSource3`. `Age` will be fetched only from `configSource3`.
       *
       * {{{
       *   val config = (string("Id") from (configSource1 orElse configSource2) |@| int("Age"))(Person.apply, Person.unapply)
       *   read(config from configSource3)
       * }}}
       */
      def orElse(that: Reader): Reader =
        Reader(
          self.sourceNames ++ that.sourceNames,
          for {
            m1 <- self.access
            m2 <- that.access
            res =
              for {
                f1 <- m1
                f2 <- m2
                res = (path: PropertyTreePath[K]) =>
                        f1(path)
                          .flatMap(tree => if (tree.isEmpty) f2(path) else ZIO.succeed(tree))
                          .orElse(f2(path))
              } yield res
          } yield res
        )

      /**
       * `<>` is an alias to `orElse`.
       * Try `this` (`configSource`), and if it fails, try `that` (`configSource`)
       *
       * For example:
       *
       * Given three configSources, `configSource1`, `configSource2` and `configSource3`, such that
       * configSource1 and configSource2 will only have `id` and `configSource3` act as a global fall-back source.
       *
       * The following config tries to fetch `Id` from configSource1, and if fails, it tries `configSource2`,
       * and if both fails it gets from `configSource3`. `Age` will be fetched only from `configSource3`.
       *
       * {{{
       *   val config = (string("Id") from (configSource1 orElse configSource2) |@| int("Age"))(Person.apply, Person.unapply)
       *   read(config from configSource3)
       * }}}
       */
      def <>(that: => Reader): ConfigSource = self orElse that
    }

    /**
     * EXPERIMENTAL
     *
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
    ): ConfigSource = {
      val tree = selectNonEmptyPropertyTree(
        getPropertyTreeFromArgs(
          args.filter(_.nonEmpty),
          keyDelimiter,
          valueDelimiter
        )
      )

      Reader(
        Set(ConfigSourceName(CommandLineArguments)),
        ZManaged.succeed(ZManaged.succeed(path => ZIO.succeed(tree.at(path))))
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
      filterKeys: String => Boolean = _ => true
    ): ConfigSource = {
      val tree =
        getPropertyTreeFromMap(constantMap, keyDelimiter, valueDelimiter, filterKeys)

      Reader(
        Set(ConfigSourceName(source)),
        ZManaged.succeed(ZManaged.succeed(path => ZIO.succeed(tree.at(path))))
      )
    }

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
      filterKeys: String => Boolean = _ => true
    ): ConfigSource = {
      val tree =
        getPropertyTreeFromMapA(map.filter({ case (k, _) => filterKeys(k) }))(
          identity,
          keyDelimiter
        )

      Reader(
        Set(ConfigSourceName(source)),
        ZManaged.succeed(ZManaged.succeed(path => ZIO.succeed(tree.at(path))))
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
      filterKeys: String => Boolean = _ => true
    ): ConfigSource = {
      val tree =
        getPropertyTreeFromProperties(property, keyDelimiter, valueDelimiter, filterKeys)

      Reader(
        Set(ConfigSourceName(source)),
        ZManaged.succeed(ZManaged.succeed(path => ZIO.succeed(tree.at(path))))
      )
    }

    /**
     * To obtain a config source directly from a property tree.
     *
     * @param tree            : PropertyTree
     * @param source          : Label the source with a name
     * @param leafForSequence : Should a single value wrapped in Leaf be considered as Sequence
     * @return
     */
    def fromPropertyTree(
      tree: PropertyTree[K, V],
      source: String
    ): ConfigSource =
      Reader(
        Set(ConfigSourceName(source)),
        ZManaged.succeed(ZManaged.succeed(path => ZIO.succeed(tree.at(path))))
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
      filterKeys: String => Boolean = _ => true
    ): ConfigSource = {
      val managed: ZManaged[Any, ReadError[K], PropertyTreePath[String] => UIO[PropertyTree[String, String]]] =
        ZManaged
          .make({
            ZIO.effect({
              println("retrieving")
              new FileInputStream(new File(filePath))
            })
          }) { r =>
            println("closing")
            ZIO.effectTotal(r.close())
          }
          .mapM { inputStream =>
            for {
              properties <- ZIO.effect {
                              val properties = new java.util.Properties()
                              properties.load(inputStream)
                              properties
                            }

              tree = getPropertyTreeFromProperties(
                       properties,
                       keyDelimiter,
                       valueDelimiter,
                       filterKeys
                     )

              fn = (path: PropertyTreePath[K]) => ZIO.succeed(tree.at(path))
            } yield fn
          }
          .mapError(throwable => ReadError.SourceError(throwable.toString))

      Reader(
        Set(ConfigSourceName(filePath)),
        ZManaged.succeed(managed)
      )
    }

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
     * With filterKeys, you can choose to filter only those keys that needs to be considered.
     *
     * Note: The delimiter '.' for keys doesn't work in system environment.
     */
    def fromSystemEnv(
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      filterKeys: String => Boolean = _ => true,
      system: System.Service = System.Service.live
    ): ConfigSource = {
      val validDelimiters = ('a' to 'z') ++ ('A' to 'Z') :+ '_'

      val managed =
        ZIO
          .accessM[System](
            _.get.envs.map(map => getPropertyTreeFromMap(map, keyDelimiter, valueDelimiter, filterKeys))
          )
          .toManaged_
          .mapError(throwable => ReadError.SourceError(throwable.toString))
          .map(tree =>
            (path: PropertyTreePath[K]) => {
              ZIO.succeed(
                tree.at(path)
              )
            }
          )
          .provideLayer(ZLayer.succeed(system))

      Reader(
        Set(ConfigSourceName(SystemEnvironment)),
        if (keyDelimiter.forall(validDelimiters.contains)) {
          ZManaged.succeed(managed)
        } else {
          // If delimiters are wrong, there isn't a need to build an inner zmanaged,
          // that's invoked per config. Instead die.
          ZManaged.fail(ReadError.SourceError(s"Invalid system key delimiter: ${keyDelimiter.get}")).orDie
        }
      )
    }

    /**
     * Consider providing keyDelimiter if you need to consider flattened config as a nested config.
     * Consider providing valueDelimiter if you need any value to be a list
     *
     * Example:
     *
     * Given:
     *
     * {{{
     *    vars in sys.props  = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERDE" = "confluent"
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
    def fromSystemProps(
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      filterKeys: String => Boolean = _ => true,
      system: System.Service = System.Service.live
    ): ConfigSource =
      Reader(
        Set(ConfigSourceName(SystemProperties)),
        ZManaged.succeed(
          ZIO
            .accessM[System](_.get.properties)
            .toManaged_
            .mapError(throwable => ReadError.SourceError(throwable.toString))
            .map(map =>
              (path: PropertyTreePath[K]) =>
                ZIO.succeed(
                  getPropertyTreeFromMap(map, keyDelimiter, valueDelimiter, filterKeys)
                    .at(path)
                )
            )
            .provideLayer(ZLayer.succeed(system))
        )
      )

    private[config] def getPropertyTreeFromArgs(
      args: List[String],
      keyDelimiter: Option[Char],
      valueDelimiter: Option[Char]
    )(implicit KS: String =:= K, VS: String =:= V): List[PropertyTree[K, V]] = {
      def unFlattenWith(
        key: String,
        tree: PropertyTree[String, String]
      ): PropertyTree[String, String] =
        keyDelimiter.fold(Record(Map(key -> tree)): PropertyTree[String, String])(value =>
          unflatten(key.split(value).toList, tree)
        )

      def toSeq[V](leaf: String): PropertyTree[String, String] =
        valueDelimiter.fold(
          Sequence(List(Leaf(leaf))): PropertyTree[String, String]
        )(c => Sequence[String, String](leaf.split(c).toList.map(Leaf(_))))

      /// CommandLine Argument Source

      case class Value(value: String)

      type KeyValue = These[Key, Value]

      import These._

      sealed trait These[+A, +B] { self =>
        def fold[C](
          f: (A, B) => C,
          g: A => C,
          h: B => C
        ): C = self match {
          case This(left)        => g(left)
          case That(right)       => h(right)
          case Both(left, right) => f(left, right)
        }
      }

      object These {
        final case class Both[A, B](left: A, right: B) extends These[A, B]
        final case class This[A](left: A)              extends These[A, Nothing]
        final case class That[B](right: B)             extends These[Nothing, B]
      }

      object KeyValue {
        def mk(s: String): Option[KeyValue] =
          splitAtFirstOccurence(s, "=") match {
            case (Some(possibleKey), Some(possibleValue)) =>
              Key.mk(possibleKey) match {
                case Some(actualKey) => Some(Both(actualKey, Value(possibleValue)))
                case None            => Some(That(Value(possibleValue)))
              }
            case (None, Some(possibleValue))              =>
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
                      .fold(List.empty[PropertyTree[String, String]])(x =>
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
                      h3.zipWithIndex.map { case (key, index) =>
                        (index, loop(List(key)))
                      }.find(_._2.nonEmpty)

                    keysAndTrees match {
                      case Some((index, trees)) =>
                        val keys = seqOption(h3.take(index).map(Key.mk))

                        keys.fold(List.empty[PropertyTree[String, String]]) { nestedKeys =>
                          trees
                            .map(tree =>
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
                    toSeq(r1.value) :: loop(h3).map(tree => unFlattenWith(l2.value, tree))
                }

              case (Some(_), None) =>
                loop(h1 :: h3)
              case (None, Some(_)) =>
                loop(h2 :: h3)
              case (None, None)    =>
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
          case Nil       => Nil
        }

      dropEmptyNode(PropertyTree.mergeAll(loop(args).map(_.bimap(KS, VS)))).map(unwrapSingletonLists(_))
    }

    private[config] def getPropertyTreeFromMapA[A](map: Map[K, A])(
      f: A => ::[V],
      keyDelimiter: Option[Char]
    ): PropertyTree[K, V] =
      selectNonEmptyPropertyTree(
        dropEmptyNode(unflatten(map.map { tuple =>
          val vectorOfKeys = keyDelimiter match {
            case Some(keyDelimiter) =>
              tuple._1.split(keyDelimiter).toVector.filterNot(_.trim == "")
            case None               => Vector(tuple._1)
          }
          vectorOfKeys -> f(tuple._2)
        })).map(unwrapSingletonLists(_))
      )

    private[config] def getPropertyTreeFromProperties(
      property: ju.Properties,
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      filterKeys: String => Boolean = _ => true
    ): PropertyTree[K, V] = {
      val mapString = property
        .stringPropertyNames()
        .asScala
        .foldLeft(Map.empty[String, String]) { (acc, a) =>
          if (filterKeys(a)) acc.updated(a, property.getProperty(a)) else acc
        }

      selectNonEmptyPropertyTree(
        dropEmptyNode(
          PropertyTree.fromStringMap(mapString, keyDelimiter, valueDelimiter)
        ).map(unwrapSingletonLists(_))
      )
    }

    private[config] def dropEmpty(tree: PropertyTree[K, V]): PropertyTree[K, V] =
      if (tree.isEmpty) PropertyTree.Empty
      else
        tree match {
          case l @ Leaf(_, _)     => l
          case Record(value)      =>
            Record(value.filterNot { case (_, v) => v.isEmpty })
          case PropertyTree.Empty => PropertyTree.Empty
          case Sequence(value)    => Sequence(value.filterNot(_.isEmpty))
        }

    private[config] def dropEmptyNode(
      trees: List[PropertyTree[K, V]]
    ): List[PropertyTree[K, V]] = {
      val res = trees.map(dropEmpty(_)).filterNot(_.isEmpty)
      if (res.isEmpty) PropertyTree.Empty :: Nil
      else res
    }

    private[config] def unwrapSingletonLists(
      tree: PropertyTree[K, V]
    ): PropertyTree[K, V] = tree match {
      case l @ Leaf(_, _)         => l
      case Record(value)          =>
        Record(value.map { case (k, v) => k -> unwrapSingletonLists(v) })
      case PropertyTree.Empty     => PropertyTree.Empty
      case Sequence(value :: Nil) => unwrapSingletonLists(value)
      case Sequence(value)        => Sequence(value.map(unwrapSingletonLists(_)))
    }

    private[config] def getPropertyTreeFromMap(
      constantMap: Map[String, String],
      keyDelimiter: Option[Char] = None,
      valueDelimiter: Option[Char] = None,
      filterKeys: String => Boolean = _ => true
    ): PropertyTree[K, V] =
      getPropertyTreeFromMapA(constantMap.filter({ case (k, _) => filterKeys(k) }))(
        x => {
          val listOfValues =
            valueDelimiter.fold(List(x))(delim => x.split(delim).toList.map(_.trim))

          ::(listOfValues.head, listOfValues.tail)
        },
        keyDelimiter
      )

    private[config] def selectNonEmptyPropertyTree(
      trees: Iterable[PropertyTree[K, V]]
    ): PropertyTree[K, V] =
      trees.find(_.nonEmpty).getOrElse(PropertyTree.empty)
  }
}
