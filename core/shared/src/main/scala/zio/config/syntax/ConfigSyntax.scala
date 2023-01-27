package zio.config.syntax

import zio.config.TupleConversion, TupleConversion._
import zio.Config
import zio.ConfigProvider
import zio.config.Read
import zio.Chunk
import zio.Trace
import zio.ZIO
import zio.IO
import zio.Config.Nested
import zio.Config.Sequence
import zio.Config.Table
import zio.Config.Fallback
import zio.Config.MapOrFail
import zio.Config.Zipped
import zio.Config.Described
import zio.Config.Lazy
import zio.Config.Error.And
import zio.Config.Error.InvalidData
import zio.Config.Error.MissingData
import zio.Config.Error.Or
import zio.Config.Error.SourceUnavailable
import zio.Config.Error.Unsupported
import zio.Config.OffsetDateTime
import zio.Config.SecretType
import zio.Config.Decimal
import zio.Config.Text
import zio.Config.Duration
import zio.Config.Bool
import zio.Config.LocalDate
import zio.Config.Constant
import zio.Config.Fail
import zio.Config.LocalTime
import zio.Config.LocalDateTime

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {
  import zio.config.VersionSpecificSupport._

  implicit class ConfigErrorOps(error: Config.Error) { self =>

    final def prettyPrint(keyDelimiter: Char = '.'): String = {

      sealed trait Segment
      sealed trait Step extends Segment

      final case class Sequential(all: List[Step])     extends Segment
      final case class Parallel(all: List[Sequential]) extends Step
      final case class Failure(lines: List[String])    extends Step

      def renderSteps(steps: List[KeyComponent]): String =
        steps
          .foldLeft(new StringBuilder()) {
            case (r, KeyComponent.KeyName(k)) => r.append(keyDelimiter).append(k.toString)
            case (r, KeyComponent.Index(i))   => r.append('[').append(i).append(']')
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
          "MissingValue" :: s"path: ${renderSteps(err.path.map(str => KeyComponent.KeyName(str)).toList)}" :: Nil

        Sequential(
          List(Failure(strings))
        )
      }

      def renderFormatError(err: Config.Error.InvalidData): Sequential = {
        val strings =
          "FormatError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path.map(str => KeyComponent.KeyName(str)).toList)}" :: Nil

        Sequential(
          List(Failure(strings))
        )
      }

      def renderConversionError(err: Config.Error.Unsupported): Sequential =
        Sequential(
          List(
            Failure(
              "ConversionError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path.map(str => KeyComponent.KeyName(str)).toList)}" :: Nil
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
  implicit class ConfigOps[A](config: zio.Config[A]) { self =>

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

    def strict: Config[A] = {

      def loop[B](config: Config[B]): Config[B] =
        config match {
          case Nested(name, config)                  => Nested(name, loop(config))
          case Sequence(config)                      => Sequence(loop(config))
          case Config.Optional(config)               => Config.Optional(loop(config))
          case Config.FallbackWith(first, second, f) => Config.FallbackWith(loop(first), loop(second), f)
          case Table(valueConfig)                    => Table(loop(valueConfig))
          case a: Fallback[B]                        => loop(a.first).orElse(loop(a.second))
          case MapOrFail(original, mapOrFail)        => MapOrFail(loop(original), a => mapOrFail(a))
          case Zipped(left, right, zippable)         => Zipped(loop(left), loop(right), zippable)
          case Described(config, description)        => Described(loop(config), description)
          case Lazy(thunk)                           => thunk().strict
          case config: Config.Primitive[B]           =>
            println("is it here?")
            println(config)
            config
        }

      loop(config)
    }

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

    def constant(value: String) =
      Config.string.mapOrFail(parsed =>
        if (parsed == value) Right(value)
        else Left(Config.Error.InvalidData(message = s"value should be a constant: ${value}"))
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
  }

  // To be moved to ZIO
  implicit class FromConfigProvider(c: ConfigProvider.type) {

    /**
     * Constructs a new ConfigProvider from a key/value (flat) provider, where
     * nesting is embedded into the string keys.
     */
    def fromIndexedFlat(flat: IndexedFlat): ConfigProvider =
      new ConfigProvider {
        import Config._

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

        def loop[A](prefix: Chunk[KeyComponent], config: Config[A])(implicit
          trace: Trace
        ): IO[Config.Error, Chunk[A]] =
          config match {
            case fallback: Fallback[A] =>
              loop(prefix, fallback.first).catchAll(e1 =>
                if (fallback.condition(e1)) loop(prefix, fallback.second).catchAll(e2 => ZIO.fail(e1 || e2))
                else ZIO.fail(e1)
              )

            case Described(config, _) => loop(prefix, config)

            case Lazy(thunk) => loop(prefix, thunk())

            case MapOrFail(original, f) =>
              loop(prefix, original).flatMap { as =>
                ZIO.foreach(as)(a => ZIO.fromEither(f(a)).mapError(_.prefixed(prefix.map(_.toString))))
              }

            case Sequence(config) =>
              for {
                keys <- flat
                          .enumerateChildren(prefix)
                          .map(_.toList.map { chunk =>
                            chunk.head.asInstanceOf[KeyComponent.Index] // FIXME
                          })

                values <-
                  ZIO.foreach(Chunk.fromIterable(keys.toSet)) { key =>
                    loop(prefix ++ Chunk(key), config)
                  }

              } yield
                (if (values.isEmpty) Chunk(Chunk.empty).asInstanceOf[Chunk[A]]
                 else Chunk(values).asInstanceOf[Chunk[A]])

            case Nested(name, config) =>
              loop(prefix ++ Chunk(KeyComponent.KeyName(name)), config)

            case table: Table[valueType]                =>
              import table.valueConfig
              null //FIXME:

            case zipped: Zipped[leftType, rightType, c] =>
              import zipped.{left, right, zippable}
              for {
                l      <- loop(prefix, left).either
                r      <- loop(prefix, right).either
                result <- (l, r) match {
                            case (Left(e1), Left(e2)) => ZIO.fail(e1 && e2)
                            case (Left(e1), Right(_)) => ZIO.fail(e1)
                            case (Right(_), Left(e2)) => ZIO.fail(e2)
                            case (Right(l), Right(r)) =>
                              val path = prefix.mkString(".")

                              def lfail(index: Int): Either[Config.Error, leftType] =
                                Left(
                                  Config.Error.MissingData(
                                    prefix.map(_.toString),
                                    s"The element at index ${index} in a sequence at ${path} was missing"
                                  )
                                )

                              def rfail(index: Int): Either[Config.Error, rightType] =
                                Left(
                                  Config.Error.MissingData(
                                    prefix.map(_.toString),
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
              ZIO.fail(Config.Error.MissingData(prefix.map(_.toString), message))

            case primitive: Primitive[A] =>
              for {
                vs     <- flat.load(prefix, primitive)
                result <- if (vs.isEmpty)
                            ZIO.fail(primitive.missingError(prefix.map(_.toString).lastOption.getOrElse("<n/a>")))
                          else ZIO.succeed(vs)
              } yield result
          }

        def load[A](config: Config[A])(implicit trace: Trace): IO[Config.Error, A] =
          loop(Chunk.empty, config).flatMap { chunk =>
            chunk.headOption match {
              case Some(a) => ZIO.succeed(a)
              case _       =>
                ZIO.fail(Config.Error.MissingData(Chunk.empty, s"Expected a single value having structure ${config}"))
            }
          }

        override def flatten: zio.ConfigProvider.Flat = null
      }

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

  }

}
