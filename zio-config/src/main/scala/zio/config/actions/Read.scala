package zio.config.actions

import zio.config.{ ConfigDescriptor, ConfigSource, ReadErrors }
import zio.{ config, ZIO }

object Read {
  // Read
  final def read[A](
    configuration: ConfigDescriptor[A]
  ): ZIO[ConfigSource[String, String], ReadErrors[String, String], A] = {
    def loop[B](
      configuration: ConfigDescriptor[B],
      previousDescription: String,
      paths: List[String]
    ): ZIO[ConfigSource[String, String], ReadErrors[String, String], B] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.access(_ => None)

        case ConfigDescriptor.Source(path, propertyType) =>
          for {
            value <- config
                      .getConfigValue[String, String](paths :+ path)
                      .mapError(ReadErrors(_))

            result <- ZIO.fromEither(
                       propertyType
                         .read(path, value)
                         .fold(r => Left(ReadErrors(r)), e => Right(e))
                     )

          } yield result

        case ConfigDescriptor.Nested(c, path) =>
          loop(c, previousDescription, paths :+ path)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, previousDescription, paths).flatMap { a =>
            ZIO.fromEither(f(a)).bimap(err => ReadErrors(err), res => res)
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, previousDescription, paths).fold(
            _ => value,
            identity
          )

        case ConfigDescriptor.Describe(c, message) =>
          loop(c, message, paths)

        case ConfigDescriptor.Optional(c) =>
          loop(c, previousDescription, paths).option

        case ConfigDescriptor.Zip(left, right) =>
          loop(left, previousDescription, paths).either
            .flatMap(
              res1 =>
                loop(right, previousDescription, paths).either.map(
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
          loop(left, previousDescription, paths).either.flatMap(
            {
              case Right(a) => ZIO.access(_ => Left(a))
              case Left(lerr) =>
                loop(right, previousDescription, paths).either.flatMap(
                  {
                    case Right(b)   => ZIO.access(_ => Right(b))
                    case Left(rerr) => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    loop(configuration, "", Nil)
  }
}
