package zio.config.syntax

import zio.Config.Error.{And, InvalidData, MissingData, Or, SourceUnavailable, Unsupported}
import zio.config.{IndexedFlat, TupleConversion}
import zio.{Chunk, Config, ConfigProvider, IO, Trace, ZIO}

import java.util.UUID
import scala.util.control.NonFatal

// Backward compatible approach to minimise the client changes
final case class Read[A](config: Config[A], configProvider: ConfigProvider)

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {

  // Backward compatible approach to minimise the client changes
  final def read[A](reader: Read[A]): IO[Config.Error, A] =
    reader.configProvider.load(reader.config)

  implicit class ConfigErrorOps(error: Config.Error) {
    self =>

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
          case And(left, right)                        => parallelSegments(left) ++ parallelSegments(right)
          case InvalidData(path, message)              => List(readErrorToSequential(readError))
          case MissingData(path, message)              => List(readErrorToSequential(readError))
          case Or(left, right)                         => List(readErrorToSequential(readError))
          case SourceUnavailable(path, message, cause) => List(readErrorToSequential(readError))
          case Unsupported(path, message)              => List(readErrorToSequential(readError))
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

  implicit class ConfigOps[A](config: zio.Config[A]) {
    self =>

    def orElseEither[B](that: Config[B]): Config[Either[A, B]] =
      config.map(Left(_)).orElse(that.map(Right(_)))

    // Important for usecases such as `SystemEnv.load(deriveConfig[A].mapKey(_.toUpperCase)` or `JsonSource.load(deriveConfig[A])`
    // where annotations can't be of any help
    def mapKey(f: String => String): Config[A] = {
      def loop[B](config: Config[B]): Config[B] =
        config match {
          case Described(config, description) => Described(loop(config), description)
          case config: FallbackWith[B]        => FallbackWith(loop(config.first), loop(config.second), config.f)
          case config: Fallback[B]            => Fallback(loop(config.first), loop(config.second))
          case Sequence(config)               => Sequence(loop(config))
          case Nested(name, config)           => Nested(f(name), loop(config))
          case MapOrFail(original, mapOrFail) => MapOrFail(loop(original), mapOrFail)
          case Table(valueConfig)             => Table(loop(valueConfig))
          case Zipped(left, right, zippable)  => Zipped(loop(left), loop(right), zippable)
          case Lazy(thunk)                    => Lazy(() => loop(thunk()))
          case primitive: Config.Primitive[B] => primitive
        }

      loop(config)
    }

    def toKebabCase: Config[A] =
      mapKey(zio.config.toKebabCase)

    def toSnakeCase: Config[A] =
      mapKey(zio.config.toSnakeCase)

    def toUpperCase: Config[A] =
      mapKey(_.toUpperCase())

    def toLowerCase: Config[A] =
      mapKey(_.toLowerCase())

    def to[B <: Product](implicit conv: TupleConversion[B, A]): Config[B] =
      config.map(
        conv.from
      )

    // To reduce the number of changes in examples
    // Example: read(config from ConfigProvider.fromMap(""))
    def from(configProvider: ConfigProvider): Read[A] =
      Read(config, configProvider)
  }

  implicit class FromConfigProviderOps(c: ConfigProvider.type) {

    def fromIndexedFlat(indexedFlat: IndexedFlat): ConfigProvider =
      new ConfigProvider {

        import Config._
        import IndexedFlat._

        def extend[A, B](
          leftDef: Int => A,
          rightDef: Int => B
        )(left: Chunk[A], right: Chunk[B]): (Chunk[A], Chunk[B]) = {
          val leftPad  = Chunk.unfold(left.length) { index =>
            if (index >= right.length) None else Some(leftDef(index) -> (index + 1))
          }
          val rightPad = Chunk.unfold(right.length) { index =>
            if (index >= left.length) None else Some(rightDef(index) -> (index + 1))
          }

          val leftExtension  = left ++ leftPad
          val rightExtension = right ++ rightPad

          (leftExtension, rightExtension)
        }

        def loop[A](prefix: IndexedFlat.ConfigPath, config: Config[A], split: Boolean)(implicit
          trace: Trace
        ): IO[Config.Error, Chunk[A]] =
          config match {
            case fallback: Fallback[A] =>
              loop(prefix, fallback.first, split).catchAll(e1 =>
                if (fallback.condition(e1)) loop(prefix, fallback.second, split).catchAll(e2 => ZIO.fail(e1 || e2))
                else ZIO.fail(e1)
              )

            case Described(config, _) => loop(prefix, config, split)

            case Lazy(thunk) => loop(prefix, thunk(), split)

            case MapOrFail(original, f) =>
              loop(prefix, original, split).flatMap { as =>
                ZIO.foreach(as)(a => ZIO.fromEither(f(a)).mapError(_.prefixed(prefix.map(_.value))))
              }

            case Sequence(config) =>
              for {
                keys <- indexedFlat
                          .enumerateChildrenIndexed(prefix)
                          .map(set =>
                            set.toList.flatMap { chunk =>
                              chunk.headOption.toList
                            }
                          )

                values <-
                  ZIO
                    .foreach(Chunk.fromIterable(keys.toSet)) { key =>
                      loop(prefix ++ Chunk(key), config, split = true)
                    }
                    .map(_.flatten)
              } yield Chunk(values)

            case Nested(name, config) =>
              loop(prefix ++ Chunk(KeyComponent.KeyName(name)), config, split)

            case table: Table[valueType] =>
              import table.valueConfig
              for {
                prefix <- ZIO.fromEither(ConfigPath.patch(indexedFlat.patch)(prefix))
                keys   <- indexedFlat.enumerateChildrenIndexed(prefix)
                values <- ZIO.foreach(Chunk.fromIterable(keys))(key => loop(prefix ++ key, valueConfig, split))
              } yield
                if (values.isEmpty) Chunk(Map.empty[String, valueType])
                else values.transpose.map(values => keys.map(_.last.value).zip(values).toMap)

            case zipped: Zipped[leftType, rightType, c] =>
              import zipped.{left, right, zippable}
              for {
                l      <- loop(prefix, left, split).either
                r      <- loop(prefix, right, split).either
                result <- (l, r) match {
                            case (Left(e1), Left(e2)) => ZIO.fail(e1 && e2)
                            case (Left(e1), Right(_)) => ZIO.fail(e1)
                            case (Right(_), Left(e2)) => ZIO.fail(e2)
                            case (Right(l), Right(r)) =>
                              val path = prefix.mkString(".")

                              def lfail(index: Int): Either[Config.Error, leftType] =
                                Left(
                                  Config.Error.MissingData(
                                    prefix.map(_.value),
                                    s"The element at index ${index} in a sequence at ${path} was missing"
                                  )
                                )

                              def rfail(index: Int): Either[Config.Error, rightType] =
                                Left(
                                  Config.Error.MissingData(
                                    prefix.map(_.value),
                                    s"The element at index ${index} in a sequence at ${path} was missing"
                                  )
                                )

                              val (ls, rs) = extend(lfail, rfail)(l.map(Right(_)), r.map(Right(_)))

                              ZIO.foreach(ls.zip(rs)) { case (l, r) =>
                                ZIO.fromEither(l).zipWith(ZIO.fromEither(r))(zippable.zip(_, _))
                              }
                          }
              } yield result

            case Constant(value) =>
              ZIO.succeed(Chunk(value))

            case Fail(message) =>
              ZIO.fail(Config.Error.MissingData(prefix.map(_.value), message))

            case primitive: Primitive[A] =>
              for {
                prefix <- ZIO.fromEither(ConfigPath.patch(indexedFlat.patch)(prefix))
                vs     <- indexedFlat.loadIndexed(prefix, primitive, split)
                result <-
                  if (vs.isEmpty)
                    ZIO.fail(primitive.missingError(prefix.lastOption.fold("<n/a>")(_.value)))
                  else ZIO.succeed(vs)
              } yield result
          }

        def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
          loop(Chunk.empty, config, false).flatMap { chunk =>
            chunk.headOption match {
              case Some(a) => ZIO.succeed(a)
              case _       =>
                ZIO.fail(Config.Error.MissingData(Chunk.empty, s"Expected a single value having structure ${config}"))
            }
          }

        override def flatten: IndexedFlat = indexedFlat
      }

    /**
     * Constructs a ConfigProvider using a map and the specified delimiter string,
     * which determines how to split the keys in the map into path segments.
     */
    def fromIndexedMap(map: Map[String, String], pathDelim: String = ".", seqDelim: String = ","): ConfigProvider =
      fromIndexedFlat(new IndexedFlat {
        val escapedSeqDelim  = java.util.regex.Pattern.quote(seqDelim)
        val escapedPathDelim = java.util.regex.Pattern.quote(pathDelim)

        def makePathString(path: Chunk[String]): String = path.mkString(pathDelim)

        def unmakePathString(pathString: String): Chunk[String] = Chunk.fromArray(pathString.split(escapedPathDelim))

        def loadIndexed[A](path: IndexedFlat.ConfigPath, primitive: Config.Primitive[A], split: Boolean)(implicit
          trace: Trace
        ): IO[Config.Error, Chunk[A]] = {
          val pathString  = makePathString(IndexedFlat.ConfigPath.toPath(path))
          val name        = path.lastOption.getOrElse(IndexedFlat.KeyComponent.KeyName("<unnamed>"))
          val valueOpt    = map.get(pathString)

          for {
            value   <- ZIO
                         .fromOption(valueOpt)
                         .mapError(_ =>
                           Config.Error.MissingData(path.map(_.value), s"Expected ${pathString} to be set in properties")
                         )
            results <- ConfigProvider.Flat.util
                         .parsePrimitive(value, path.map(_.value), name.value, primitive, escapedSeqDelim, split)
          } yield results
        }

        def enumerateChildrenIndexed(path: IndexedFlat.ConfigPath)(implicit
          trace: Trace
        ): IO[Config.Error, Set[IndexedFlat.ConfigPath]] =
          ZIO.succeed {
            val keyPaths: Chunk[IndexedFlat.ConfigPath] = Chunk
              .fromIterable(map.keys)
              .map(unmakePathString)
              .map(IndexedFlat.ConfigPath.fromPath)

            keyPaths.filter(_.startsWith(path)).map(_.drop(path.length).take(1)).toSet
          }

      })

  }

  // To be moved to ZIO
  implicit class FromConfigOps(c: Config.type) {

    def byte: Config[Byte] =
      Config.string.mapOrFail(text =>
        try Right(text.toByte)
        catch {
          case NonFatal(e) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected an byte, but found ${text}"))
        }
      )

    def collectAll[A](head: => Config[A], tail: Config[A]*): Config[List[A]] =
      tail.reverse
        .map(Config.defer(_))
        .foldLeft[Config[(A, List[A])]](
          Config
            .defer(head)
            .map((a: A) => (a, Nil))
        )((b: Config[(A, List[A])], a: Config[A]) =>
          (b.zip[A](a)
            .map({ case (first, tail, a) =>
              (first, a :: tail)
            }))
        )
        .map { case (a, t) => a :: t }

    def constant(value: String): Config[String]                              =
      Config.string.mapOrFail(parsed =>
        if (parsed == value) Right(value)
        else Left(Config.Error.InvalidData(message = s"value should be a constant: ${value}"))
      ) ?? s"The value should be ${value}"

    def long: Config[Long] =
      Config.bigInt.map(_.toLong)

    def short: Config[Short] =
      Config.string.mapOrFail(text =>
        try Right(text.toShort)
        catch {
          case NonFatal(e) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected a short, but found ${text}"))
        }
      )

    def uuid: Config[UUID] =
      Config.string.mapOrFail(text =>
        try Right(UUID.fromString(text))
        catch {
          case NonFatal(e) =>
            Left(Config.Error.InvalidData(Chunk.empty, s"Expected a uuid, but found ${text}"))
        }
      )
  }

  implicit class ChunkOps[A](chunk: Chunk[A]) {
    def mapLast(f: A => A): Chunk[A] =
      chunk.lastOption match {
        case Some(value) => chunk.dropRight(1) :+ f(value)
        case None        => chunk
      }
  }

}
