package zio.config.syntax

import zio.config.TupleConversion, TupleConversion._
import zio.Config
import zio.ConfigProvider
import zio.Chunk
import zio.Config.Error.And
import zio.Config.Error.InvalidData
import zio.Config.Error.MissingData
import zio.Config.Error.Or
import zio.Config.Error.SourceUnavailable
import zio.Config.Error.Unsupported
import zio.IO
import scala.util.control.NonFatal
import java.util.UUID

// Backward compatible approach to minimise the client changes
final case class Read[A](config: Config[A], configProvider: ConfigProvider)

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {

  // Backward compatible approach to minimise the client changes
  final def read[A](reader: Read[A]): IO[Config.Error, A] =
    reader.configProvider.load(reader.config)

  import zio.config.VersionSpecificSupport._

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
          .foldLeft(new StringBuilder()) {
            case (r, k) => r.append(keyDelimiter).append(k)
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
          case Fail(message)                  => Fail(message)
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

  // To be moved to ZIO
  implicit class FromConfigTypesafe(c: Config.type) {

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

    def constant(value: String): Config[String] =
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
        case None => chunk
      }
  }

}
