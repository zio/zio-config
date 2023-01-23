package zio.config.syntax

import zio.config.TupleConversion, TupleConversion._
import zio.Config
import zio.ConfigProvider
import zio.config.Read
import zio.Chunk
import zio.Trace
import zio.ZIO
import zio.IO
import zio.config.syntax.KeyComponent.Index
import zio.config.syntax.KeyComponent.KeyName
import zio.Config.Nested
import zio.Config.Sequence
import zio.Config.Table
import zio.Config.Fallback
import zio.Config.MapOrFail
import zio.Config.Zipped
import zio.Config.Described
import zio.Config.Lazy
import zio.Config.LocalTime
import zio.Config.LocalDate
import zio.Config.SecretType
import zio.Config.OffsetDateTime
import zio.Config.Bool
import zio.Config.Decimal
import zio.Config.Text
import zio.Config.Constant
import zio.Config.Fail
import zio.Config.LocalDateTime
import zio.Config.Duration
import zio.Unsafe

// To be moved to ZIO ?
// Or may be zio-config can be considered as an extension to ZIO
trait ConfigSyntax {
  import zio.config.VersionSpecificSupport._

  implicit class ConfigOps[A](config: zio.Config[A]) { self =>

    def strict: Config[A] = {
      def loop[B](config: Config[B]): Config[B] =
        config match {
          case Nested(name, config)           => Nested(name, config.strict)
          case Sequence(config)               => Sequence(config.strict)
          case Table(valueConfig)             => Table(valueConfig.strict)
          case a: Fallback[_]                 => a.first.strict.orElse(a.second.strict)
          case MapOrFail(original, mapOrFail) => MapOrFail(original.strict, mapOrFail)
          case Zipped(left, right, zippable)  => Zipped(left.strict, right.strict, zippable)
          case Described(config, description) => Described(config.strict, description)
          case Lazy(thunk)                    => thunk()
          case a                              => a
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

              } yield (if (values.isEmpty) Chunk(Chunk.empty) else Chunk(values))

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
