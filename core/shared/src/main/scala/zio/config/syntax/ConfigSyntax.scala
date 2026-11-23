package zio.config.syntax

import zio.Config.Error.{And, InvalidData, MissingData, Or, SourceUnavailable, Unsupported}
import zio.config.TupleConversion
import zio.config.syntax.ConfigSyntax.ConfigProviderZIO
import zio.{Chunk, Config, ConfigProvider, IO, Task}

import java.util.UUID
import scala.util.control.NonFatal

// Backward compatible approach to minimise the client changes
/**
 * A `Config` paired with the `ConfigProvider` to load it from, as built by `config.from(provider)`. Pass it to `read`
 * to load the value.
 */
final case class Read[A](config: Config[A], configProvider: ConfigProvider)

/**
 * A `Config` paired with an effect that builds the `ConfigProvider`, as built by `config.from(task)`. Pass it to `read`
 * to build the provider and load the value.
 */
final case class ReadZIO[A](config: Config[A], configProvider: ConfigProviderZIO)

/**
 * Holds the `ConfigProviderZIO` alias and the `toConfigProviderZIO` syntax. The `zio.config` package object doesn't
 * export them: import `zio.config.syntax.ConfigSyntax._` to use them.
 */
object ConfigSyntax {

  /**
   * An effect that builds a `ConfigProvider` and can fail with a `Config.Error`.
   */
  type ConfigProviderZIO = IO[Config.Error, ConfigProvider]

  /**
   * Adds `toConfigProviderZIO` to a `Task[ConfigProvider]`.
   */
  implicit class ConfigProviderZIOOps(buildConfigProvider: Task[ConfigProvider]) {

    /**
     * Turns a failure of this effect into a `Config.Error.Unsupported`, so the effect fits where a `Config.Error` is
     * expected.
     *
     * {{{
     * import zio._
     * import zio.config.syntax.ConfigSyntax._
     *
     * val provider: Task[ConfigProvider] =
     *   ZIO.attempt(ConfigProvider.fromMap(Map("port" -> "8080")))
     *
     * val typed: IO[Config.Error, ConfigProvider] = provider.toConfigProviderZIO
     * }}}
     */
    def toConfigProviderZIO: ConfigProviderZIO =
      buildConfigProvider.mapError(throwable => Config.Error.Unsupported(message = throwable.toString))
  }
}

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
/**
 * The syntax that `import zio.config._` adds: `read`, `prettyPrint` on `Config.Error`, key mapping, `orElseEither`,
 * `to` and `from` on `Config`, and more constructors on the `Config` companion. The `zio.config` package object extends
 * this trait.
 *
 * @see
 *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
 */
trait ConfigSyntax {

  // Backward compatible approach to minimise the client changes
  /**
   * Loads `reader.config` from `reader.configProvider`. Build the `Read` with `config.from(provider)`.
   *
   * {{{
   * import zio._
   * import zio.config._
   *
   * val port: IO[Config.Error, Int] =
   *   read(Config.int("port").from(ConfigProvider.fromMap(Map("port" -> "8080"))))
   * // succeeds with 8080
   * }}}
   *
   * @see
   *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
   */
  final def read[A](reader: Read[A]): IO[Config.Error, A] =
    reader.configProvider.load(reader.config)

  /**
   * Runs `reader.configProvider` to build the provider, then loads `reader.config` from it. Build the `ReadZIO` with
   * `config.from(task)`; a failure of the task becomes a `Config.Error.Unsupported`.
   *
   * {{{
   * import zio._
   * import zio.config._
   *
   * val provider: Task[ConfigProvider] =
   *   ZIO.attempt(ConfigProvider.fromMap(Map("timeout" -> "30")))
   *
   * val timeout: IO[Config.Error, Int] = read(Config.int("timeout").from(provider))
   * // succeeds with 30
   * }}}
   */
  final def read[A](reader: ReadZIO[A]): IO[Config.Error, A] =
    reader.configProvider.flatMap(_.load(reader.config))

  /**
   * Adds `prettyPrint` to `Config.Error`.
   */
  implicit class ConfigErrorOps(error: Config.Error) {
    self =>

    /**
     * Renders this error as text for a person to read. The first line is `ReadError:`. A single error takes a few
     * lines: its kind (`MissingValue`, `FormatError`, `ConversionError` or `SourceError`), its path and its message.
     * Errors joined with `&&` (all of them happened, as when both sides of a `zip` fail) are drawn side by side in
     * columns. Errors joined with `||` (each alternative of an `orElse` failed) are listed one below the other.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val error: Config.Error =
     *   Config.Error.MissingData(Chunk("server", "port"), "Expected server.port to be set in properties")
     *
     * val text: String = error.prettyPrint()
     * // ReadError:
     * // MissingValue
     * // path: server.port
     * // Expected server.port to be set in properties
     * }}}
     *
     * @param keyDelimiter
     *   the character that joins the parts of each path on the `path:` lines; it doesn't change the messages
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    final def prettyPrint(keyDelimiter: Char = '.'): String = {

      sealed trait Segment
      sealed trait Step extends Segment

      final case class Sequential(all: List[Step])     extends Segment
      final case class Parallel(all: List[Sequential]) extends Step
      final case class Failure(lines: List[String])    extends Step

      def renderSteps(steps: List[String]): String =
        steps
          .foldLeft(new StringBuilder()) { case (r, k) =>
            r.append(keyDelimiter).append(k)
          }
          .delete(0, 1)
          .toString()

      def prefixBlock(values: List[String], p1: String, p2: String): List[String] =
        values match {
          case Nil          => Nil
          case head :: tail =>
            (p1 + head) :: tail.map(p2 + _)
        }

      def parallelSegments(readError: Config.Error): List[Sequential] =
        readError match {
          case And(left, right)           => parallelSegments(left) ++ parallelSegments(right)
          case InvalidData(_, _)          => List(readErrorToSequential(readError))
          case MissingData(_, _)          => List(readErrorToSequential(readError))
          case Or(_, _)                   => List(readErrorToSequential(readError))
          case SourceUnavailable(_, _, _) => List(readErrorToSequential(readError))
          case Unsupported(_, _)          => List(readErrorToSequential(readError))
        }

      def linearSegments(readError: Config.Error): List[Step] =
        readError match {
          case Config.Error.Or(left, right) => linearSegments(left) ++ linearSegments(right)
          case _                            => readErrorToSequential(readError).all
        }

      def renderMissingValue(err: Config.Error.MissingData): Sequential = {
        val strings =
          "MissingValue" :: s"path: ${renderSteps(err.path.toList)}" :: s"${err.message}" :: Nil

        Sequential(
          List(Failure(strings))
        )
      }

      def renderFormatError(err: Config.Error.InvalidData): Sequential = {
        val strings =
          "FormatError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path.toList)}" :: Nil

        Sequential(
          List(Failure(strings))
        )
      }

      def renderConversionError(err: Config.Error.Unsupported): Sequential =
        Sequential(
          List(
            Failure(
              "ConversionError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path.toList)}" :: Nil
            )
          )
        )

      def renderSourceError(err: Config.Error.SourceUnavailable): Sequential =
        Sequential(
          List(
            Failure(s"SourceError: ${err.message}" :: Nil)
          )
        )

      def readErrorToSequential(readError: Config.Error): Sequential =
        readError match {
          case r: MissingData       => renderMissingValue(r)
          case r: SourceUnavailable => renderSourceError(r)
          case r: Unsupported       => renderConversionError(r)
          case t: Or                => Sequential(linearSegments(t))
          case b: And               => Sequential(List(Parallel(parallelSegments(b))))
          case f: InvalidData       => renderFormatError(f)
        }

      def format(segment: Segment): List[String] =
        segment match {
          case Failure(lines)  =>
            prefixBlock(lines, "─", " ")
          case Parallel(all)   =>
            List(("══╦" * (all.size - 1)) + "══╗") ++
              all.foldRight[List[String]](Nil) { case (current, acc) =>
                prefixBlock(acc, "  ║", "  ║") ++
                  prefixBlock(format(current), "  ", "  ")
              }
          case Sequential(all) =>
            all.flatMap { segment =>
              List("║") ++
                prefixBlock(format(segment), "╠", "║")
            } ++ List("▼")
        }

      val sequence = readErrorToSequential(error)

      ("ReadError:" :: {
        sequence match {
          // use simple report for single failures
          case Sequential(List(Failure(readError))) => readError

          case _ => format(sequence).updated(0, "╥")
        }
      }).mkString(System.lineSeparator())
    }

  }

  import zio.{Config, ConfigProvider}, Config._

  /**
   * Adds key mapping, `orElseEither`, `to` and `from` to a `Config`.
   */
  implicit class ConfigOps[A](config: zio.Config[A]) {
    self =>

    /**
     * Reads this config and returns its value in a `Left`. If that fails, because the value is missing or invalid,
     * reads `that` and returns its value in a `Right`. When both fail, the error holds both failures.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val portOrSocket: Config[Either[Int, String]] =
     *   Config.int("port").orElseEither(Config.string("socket"))
     *
     * val result: IO[Config.Error, Either[Int, String]] =
     *   ConfigProvider.fromMap(Map("socket" -> "/var/run/app.sock")).load(portOrSocket)
     * // succeeds with Right(/var/run/app.sock)
     * }}}
     *
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    def orElseEither[B](that: Config[B]): Config[Either[A, B]] =
      config.map(Left(_)).orElse(that.map(Right(_)))

    /**
     * Returns a config that asks for each key of this config under the name `f(name)`. It changes every name given to
     * `nested`, which includes the names passed to constructors such as `Config.int("port")` and `Config.listOf`. The
     * keys that a `Config.table` finds in the source are not changed. Use it when the source names keys differently
     * from the config, as with upper-case environment variables or a derived config.
     *
     * A known bug: an `optional` config inside, including an `Option` field of a derived config, loses its rule of
     * falling back to `None` only when the value is missing. After `mapKey`, an invalid value reads as `None` instead
     * of failing.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val server: Config[(String, Int)] =
     *   Config.string("host").zip(Config.int("port")).nested("server")
     *
     * val result: IO[Config.Error, (String, Int)] =
     *   ConfigProvider
     *     .fromMap(Map("SERVER.HOST" -> "localhost", "SERVER.PORT" -> "8080"))
     *     .load(server.mapKey(_.toUpperCase))
     * // succeeds with (localhost,8080)
     * }}}
     *
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    def mapKey(f: String => String): Config[A] = {
      def loop[B](config: Config[B]): Config[B] =
        config match {
          case Described(config, description) => Described(loop(config), description)
          case config: FallbackWith[B]        => FallbackWith(loop(config.first), loop(config.second), config.f)
          case config: Fallback[B]            => Fallback(loop(config.first), loop(config.second))
          case Sequence(config)               => Sequence(loop(config))
          case Switch(config, map)            => Switch(config, map.map { case (k, v) => k -> loop(v) })
          case Nested(name, config)           => Nested(f(name), loop(config))
          case MapOrFail(original, mapOrFail) => MapOrFail(loop(original), mapOrFail)
          case Table(valueConfig)             => Table(loop(valueConfig))
          case Zipped(left, right, zippable)  => Zipped(loop(left), loop(right), zippable)
          case Lazy(thunk)                    => Lazy(() => loop(thunk()))
          case primitive: Config.Primitive[B] => primitive
        }

      loop(config)
    }

    /**
     * Returns a config that asks for each key in kebab-case, converted with `zio.config.toKebabCase`, so `maxPoolSize`
     * reads the key `max-pool-size`. It changes the same keys as `mapKey`, and shares its known bug with `optional`.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val pool: Config[(Int, Int)] =
     *   Config.int("maxPoolSize").zip(Config.int("minIdle")).nested("dbPool").toKebabCase
     *
     * val result: IO[Config.Error, (Int, Int)] =
     *   ConfigProvider.fromMap(Map("db-pool.max-pool-size" -> "16", "db-pool.min-idle" -> "2")).load(pool)
     * // succeeds with (16,2)
     * }}}
     */
    def toKebabCase: Config[A] =
      mapKey(zio.config.toKebabCase)

    /**
     * Returns a config that asks for each key in snake_case, converted with `zio.config.toSnakeCase`, so `maxPoolSize`
     * reads the key `max_pool_size`. It changes the same keys as `mapKey`, and shares its known bug with `optional`.
     */
    def toSnakeCase: Config[A] =
      mapKey(zio.config.toSnakeCase)

    /**
     * Returns a config that asks for each key in upper case, so `port` reads the key `PORT`. It changes the same keys
     * as `mapKey`, and shares its known bug with `optional`.
     */
    def toUpperCase: Config[A] =
      mapKey(_.toUpperCase())

    /**
     * Returns a config that asks for each key in lower case, so `dbUrl` reads the key `dburl`. It changes the same keys
     * as `mapKey`, and shares its known bug with `optional`.
     */
    def toLowerCase: Config[A] =
      mapKey(_.toLowerCase())

    /**
     * Converts the value of this config into the case class `B`, using an implicit [[zio.config.TupleConversion]]. With
     * the instances that `import zio.config._` derives, the value must be the tuple of the field types of `B` in field
     * order, as `zip` builds it, or the field's type for a case class with one field. A tuple in another order, or a
     * `B` that isn't a case class, doesn't compile.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * final case class Database(host: String, port: Int)
     *
     * val database: Config[Database] =
     *   Config.string("host").zip(Config.int("port")).to[Database]
     * }}}
     *
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    def to[B <: Product](implicit conv: TupleConversion[B, A]): Config[B] =
      config.map(
        conv.from
      )

    // To reduce the number of changes in examples
    // Example: read(config from ConfigProvider.fromMap(""))
    /**
     * Pairs this config with `configProvider` into a [[zio.config.syntax.Read]], to pass to `read`.
     */
    def from(configProvider: ConfigProvider): Read[A] =
      Read(config, configProvider)

    import ConfigSyntax.ConfigProviderZIOOps

    /**
     * Pairs this config with an effect that builds the provider into a [[zio.config.syntax.ReadZIO]], to pass to
     * `read`. A failure of the effect becomes a `Config.Error.Unsupported`.
     */
    def from(configProvider: Task[ConfigProvider]): ReadZIO[A] =
      ReadZIO(config, configProvider.toConfigProviderZIO)
  }

  /**
   * Syntax for the `ConfigProvider` companion. It currently adds no methods.
   */
  implicit class FromConfigProviderOps(c: ConfigProvider.type) {

    // Disabled until next version of ZIO: https://github.com/zio/zio-config/blob/avoid_custom_index_until_3/README.md#indexed-map-array-datatype-and-a-some-implementation-notes
    //    private def fromIndexedFlat(indexedFlat: IndexedFlat): ConfigProvider =
    //      new ConfigProvider {
    //
    //        import Config._
    //        import IndexedFlat._
    //
    //        def extend[A, B](
    //          leftDef: Int => A,
    //          rightDef: Int => B
    //        )(left: Chunk[A], right: Chunk[B]): (Chunk[A], Chunk[B]) = {
    //          val leftPad  = Chunk.unfold(left.length) { index =>
    //            if (index >= right.length) None else Some(leftDef(index) -> (index + 1))
    //          }
    //          val rightPad = Chunk.unfold(right.length) { index =>
    //            if (index >= left.length) None else Some(rightDef(index) -> (index + 1))
    //          }
    //
    //          val leftExtension  = left ++ leftPad
    //          val rightExtension = right ++ rightPad
    //
    //          (leftExtension, rightExtension)
    //        }
    //
    //        def loop[A](prefix: IndexedFlat.ConfigPath, config: Config[A], split: Boolean)(implicit
    //          trace: Trace
    //        ): IO[Config.Error, Chunk[A]] =
    //          config match {
    //            case fallback: Fallback[A] =>
    //              loop(prefix, fallback.first, split).catchAll(e1 =>
    //                if (fallback.condition(e1)) loop(prefix, fallback.second, split).catchAll(e2 => ZIO.fail(e1 || e2))
    //                else ZIO.fail(e1)
    //              )
    //
    //            case Described(config, _) => loop(prefix, config, split)
    //
    //            case Lazy(thunk) => loop(prefix, thunk(), split)
    //
    //            case MapOrFail(original, f) =>
    //              loop(prefix, original, split).flatMap { as =>
    //                ZIO.foreach(as)(a => ZIO.fromEither(f(a)).mapError(_.prefixed(prefix.map(_.value))))
    //              }
    //
    //            case Sequence(config) =>
    //              for {
    //                keys <- indexedFlat
    //                          .enumerateChildrenIndexed(prefix)
    //                          .flatMap { set =>
    //                            ZIO.foreach(set.toList) { configPath =>
    //                              configPath.headOption match {
    //                                case Some(value) =>
    //                                  value match {
    //                                    case index @ KeyComponent.Index(_) => ZIO.succeed(List(index))
    //                                    case KeyComponent.KeyName(_)       =>
    //                                      ZIO.fail(
    //                                        Config.Error.InvalidData(
    //                                          prefix.map(_.value),
    //                                          "A list key should be always followed by an index which is an integer to consider the values as a list. Example: The key `employees` is a list only if there is exists `employees[0]`, `employees[1]` etc as keys"
    //                                        )
    //                                      )
    //                                  }
    //                                case None        =>
    //                                  ZIO.succeed(Nil)
    //                              }
    //                            }
    //                          }
    //                          .map(_.flatten.sortBy(_.index))
    //
    //                values <-
    //                  ZIO
    //                    .foreach(Chunk.fromIterable(keys.toSet)) { key =>
    //                      loop(prefix ++ Chunk(key), config, split = true)
    //                    }
    //                    .map(_.flatten)
    //              } yield Chunk(values)
    //
    //            case Nested(name, config) =>
    //              loop(prefix ++ Chunk(KeyComponent.KeyName(name)), config, split)
    //
    //            case table: Table[valueType] =>
    //              import table.valueConfig
    //              for {
    //                prefix <- ZIO.fromEither(ConfigPath.patch(indexedFlat.patch)(prefix))
    //                keys   <- indexedFlat.enumerateChildrenIndexed(prefix)
    //                values <- ZIO.foreach(Chunk.fromIterable(keys))(key => loop(prefix ++ key, valueConfig, split))
    //              } yield
    //                if (values.isEmpty) Chunk(Map.empty[String, valueType])
    //                else values.transpose.map(values => keys.map(_.last.value).zip(values).toMap)
    //
    //            case zipped: Zipped[leftType, rightType, c] =>
    //              import zipped.{left, right, zippable}
    //              for {
    //                l      <- loop(prefix, left, split).either
    //                r      <- loop(prefix, right, split).either
    //                result <- (l, r) match {
    //                            case (Left(e1), Left(e2)) => ZIO.fail(e1 && e2)
    //                            case (Left(e1), Right(_)) => ZIO.fail(e1)
    //                            case (Right(_), Left(e2)) => ZIO.fail(e2)
    //                            case (Right(l), Right(r)) =>
    //                              val path = prefix.mkString(".")
    //
    //                              def lfail(index: Int): Either[Config.Error, leftType] =
    //                                Left(
    //                                  Config.Error.MissingData(
    //                                    prefix.map(_.value),
    //                                    s"The element at index ${index} in a sequence at ${path} was missing"
    //                                  )
    //                                )
    //
    //                              def rfail(index: Int): Either[Config.Error, rightType] =
    //                                Left(
    //                                  Config.Error.MissingData(
    //                                    prefix.map(_.value),
    //                                    s"The element at index ${index} in a sequence at ${path} was missing"
    //                                  )
    //                                )
    //
    //                              val (ls, rs) = extend(lfail, rfail)(l.map(Right(_)), r.map(Right(_)))
    //
    //                              ZIO.foreach(ls.zip(rs)) { case (l, r) =>
    //                                ZIO.fromEither(l).zipWith(ZIO.fromEither(r))(zippable.zip(_, _))
    //                              }
    //                          }
    //              } yield result
    //
    //            case Constant(value) =>
    //              ZIO.succeed(Chunk(value))
    //
    //            case Fail(message) =>
    //              ZIO.fail(Config.Error.MissingData(prefix.map(_.value), message))
    //
    //            case primitive: Primitive[A] =>
    //              for {
    //                prefix <- ZIO.fromEither(ConfigPath.patch(indexedFlat.patch)(prefix))
    //                vs     <- indexedFlat.loadIndexed(prefix, primitive, split)
    //                result <-
    //                  if (vs.isEmpty)
    //                    ZIO.fail(primitive.missingError(prefix.lastOption.fold("<n/a>")(_.value)))
    //                  else ZIO.succeed(vs)
    //              } yield result
    //          }
    //
    //        def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
    //          loop(Chunk.empty, config, false).flatMap { chunk =>
    //            chunk.headOption match {
    //              case Some(a) => ZIO.succeed(a)
    //              case _       =>
    //                ZIO.fail(Config.Error.MissingData(Chunk.empty, s"Expected a single value having structure ${config}"))
    //            }
    //          }
    //
    //        override def flatten: IndexedFlat = indexedFlat
    //      }

    // Disabled until next version of ZIO: https://github.com/zio/zio-config/blob/avoid_custom_index_until_3/README.md#indexed-map-array-datatype-and-a-some-implementation-notes
    //    private def fromIndexedMap(map: Map[String, String], pathDelim: String = "."): ConfigProvider =
    //      fromIndexedFlat(new IndexedFlat {
    //        val escapedPathDelim = java.util.regex.Pattern.quote(pathDelim)
    //
    //        def makePathString(path: Chunk[String]): String = path.mkString(pathDelim)
    //
    //        def unmakePathString(pathString: String): Chunk[String] = Chunk.fromArray(pathString.split(escapedPathDelim))
    //
    //        def loadIndexed[A](path: IndexedFlat.ConfigPath, primitive: Config.Primitive[A], split: Boolean)(implicit
    //          trace: Trace
    //        ): IO[Config.Error, Chunk[A]] = {
    //          val pathString = makePathString(IndexedFlat.ConfigPath.toPath(path))
    //          val valueOpt   = map.get(pathString)
    //
    //          for {
    //            value   <- ZIO
    //                         .fromOption(valueOpt)
    //                         .mapError(_ =>
    //                           Config.Error.MissingData(path.map(_.value), s"Expected ${pathString} to be set in properties")
    //                         )
    //            results <-
    //              ZIO
    //                .fromEither(primitive.parse(value))
    //                .map(Chunk(_))
    //                .mapError(_.prefixed(path.map(_.value)))
    //          } yield results
    //        }
    //
    //        def enumerateChildrenIndexed(path: IndexedFlat.ConfigPath)(implicit
    //          trace: Trace
    //        ): IO[Config.Error, Set[IndexedFlat.ConfigPath]] =
    //          ZIO.succeed {
    //            val keyPaths: Chunk[IndexedFlat.ConfigPath] = Chunk
    //              .fromIterable(map.keys)
    //              .map(unmakePathString)
    //              .map(IndexedFlat.ConfigPath.fromPath)
    //
    //            keyPaths.filter(_.startsWith(path)).map(_.drop(path.length).take(1)).toSet
    //          }
    //
    //      })

  }

  // To be moved to ZIO
  /**
   * Adds `byte`, `short`, `uuid`, `constant`, `collectAll` and `long` to the `Config` companion.
   */
  implicit class FromConfigOps(c: Config.type) {

    /**
     * A config that reads a `Byte`, from `-128` to `127`. Text that `String#toByte` rejects, such as `300` or `abc`,
     * fails with a `Config.Error.InvalidData`.
     */
    def byte: Config[Byte] =
      Config.string.mapOrFail(text =>
        try Right(text.toByte)
        catch {
          case NonFatal(_) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected an byte, but found ${text}"))
        }
      )

    /**
     * Reads `head` and each config in `tail`, and returns their values in a list, in the same order. When several of
     * them fail, the error holds each failure.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val ports: Config[List[Int]] =
     *   Config.collectAll(Config.int("http"), Config.int("https"), Config.int("admin"))
     *
     * val result: IO[Config.Error, List[Int]] =
     *   ConfigProvider.fromMap(Map("http" -> "80", "https" -> "443", "admin" -> "9000")).load(ports)
     * // succeeds with List(80, 443, 9000)
     * }}}
     *
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    def collectAll[A](head: => Config[A], tail: Config[A]*): Config[List[A]] =
      tail.reverse
        .map(Config.defer(_))
        .foldLeft[Config[(A, List[A])]](
          Config
            .defer(head)
            .map((a: A) => (a, Nil))
        )((b: Config[(A, List[A])], a: Config[A]) =>
          (b.zip[A](a).map { case (first, tail, a) =>
            (first, a :: tail)
          })
        )
        .map { case (a, t) => a :: t }

    /**
     * A config that reads text and succeeds with `value` when the text equals `value` exactly, case included. Other
     * text fails with a `Config.Error.InvalidData` whose message is `value should be a constant: ` followed by `value`.
     * The config is described as `The value should be ` followed by `value`. Combine it with `orElseEither` to accept
     * one of several fixed words.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val role: Config[String] =
     *   Config.constant("primary").orElseEither(Config.constant("replica")).map(_.merge).nested("role")
     *
     * val result: IO[Config.Error, String] =
     *   ConfigProvider.fromMap(Map("role" -> "replica")).load(role)
     * // succeeds with replica
     * }}}
     *
     * @see
     *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
     */
    def constant(value: String): Config[String] =
      Config.string.mapOrFail(parsed =>
        if (parsed == value) Right(value)
        else Left(Config.Error.InvalidData(message = s"value should be a constant: ${value}"))
      ) ?? s"The value should be ${value}"

    /**
     * A config that reads a `Long` through `Config.bigInt`. ZIO's `Config` companion has its own `long` with the same
     * definition, so `Config.long` calls ZIO's, and this one is reached as `FromConfigOps(Config).long`. A known bug,
     * in both: a number outside the `Long` range wraps around, so `9223372036854775808` reads as
     * `-9223372036854775808`.
     */
    def long: Config[Long] =
      Config.bigInt.map(_.toLong)

    /**
     * A config that reads a `Short`, from `-32768` to `32767`. Text that `String#toShort` rejects, such as `40000` or
     * text with a leading space, fails with a `Config.Error.InvalidData`.
     */
    def short: Config[Short] =
      Config.string.mapOrFail(text =>
        try Right(text.toShort)
        catch {
          case NonFatal(_) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected a short, but found ${text}"))
        }
      )

    /**
     * A config that reads a `java.util.UUID` with `UUID.fromString`. Text that `UUID.fromString` rejects fails with a
     * `Config.Error.InvalidData`.
     */
    def uuid: Config[UUID] =
      Config.string.mapOrFail(text =>
        try Right(UUID.fromString(text))
        catch {
          case NonFatal(_) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected a uuid, but found ${text}"))
        }
      )
  }

  /**
   * Adds `mapLast` to a `Chunk`.
   */
  implicit class ChunkOps[A](chunk: Chunk[A]) {

    /**
     * Returns this chunk with `f` applied to its last element, or the chunk unchanged when it is empty.
     */
    def mapLast(f: A => A): Chunk[A] =
      chunk.lastOption match {
        case Some(value) => chunk.dropRight(1) :+ f(value)
        case None        => chunk
      }
  }

}
