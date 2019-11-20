package zio.config.actions

import zio.config.ReadErrors.ReadError
import zio.config.{ ConfigDescriptor, ConfigErrors, ConfigSource, PropertyType, ReadErrors }
import zio.{ IO, ZIO }

object Read {
  // Read
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ConfigErrors[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): IO[ConfigErrors[K, V1], B] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.succeed(None)

        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          for {
            value <- source.getConfigValue(paths :+ path)
            result <- ZIO.fromEither(
                       propertyType
                         .read(value)
                         .fold(
                           r =>
                             Left(
                               ReadErrors(
                                 ReadError.ParseError(paths :+ path, r.value, r.typeInfo)
                               )
                             ),
                           e => Right(e)
                         )
                     )
          } yield result

        case ConfigDescriptor.Nested(c, path) =>
          loop(c, paths :+ path)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, paths).flatMap { a =>
            ZIO
              .fromEither(f(a))
              .bimap(
                err =>
                  ReadErrors(
                    ReadError.FatalError(paths, new RuntimeException(err))
                  ),
                res => res
              )
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, paths).fold(
            _ => value,
            identity
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case ConfigDescriptor.Optional(c) =>
          loop(c, paths).option

        case ConfigDescriptor.Zip(left, right) =>
          loop(left, paths).either
            .flatMap(
              res1 =>
                loop(right, paths).either.map(
                  res2 =>
                    (res1, res2) match {
                      case (Right(a), Right(b))     => Right((a, b))
                      case (Left(a), Right(_))      => Left(a)
                      case (Right(_), Left(error))  => Left(error)
                      case (Left(err1), Left(err2)) => Left(ReadErrors.concat(err1, err2))
                    }
                )
            )
            .absolve

        case ConfigDescriptor.OrElseEither(left, right) =>
          loop(left, paths).either.flatMap(
            {
              case Right(a) => ZIO.access(_ => Left(a))
              case Left(lerr) =>
                loop(right, paths).either.flatMap(
                  {
                    case Right(b)   => ZIO.access(_ => Right(b))
                    case Left(rerr) => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    loop(configuration, Vector.empty[K])
  }
}
