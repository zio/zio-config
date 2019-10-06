package zio.config.actions

import zio.config.ReadErrors
import zio.config.ConfigDescriptor
import zio.config.{ ConfigSource }
import zio.{ config, ZIO }

object Read {
  // Read
  final def read[A](configuration: ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, A] = {
    def loop[B](
      configuration: ConfigDescriptor[B],
      previousDescription: String
    ): ZIO[ConfigSource, ReadErrors, B] =
      configuration match {
        case ConfigDescriptor.Succeed(v) => ZIO.access(_ => v)

        case ConfigDescriptor.Source(path, propertyType) =>
          for {
            value <- config
                      .getConfigValue(path)
                      .mapError(err => ReadErrors(err))

            result <- ZIO.fromEither(
                       propertyType
                         .read(path, value)
                         .fold(r => Left(ReadErrors(r)), e => Right(e))
                     )

          } yield result

        case ConfigDescriptor.MapEither(c, f, _) =>
          loop(c, previousDescription).flatMap { a =>
            ZIO.fromEither(f(a)).bimap(err => ReadErrors(err), res => res)
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, previousDescription).fold(
            _ => value,
            identity
          )

        case ConfigDescriptor.Describe(c, message) =>
          loop(c, message)

        case ConfigDescriptor.Optional(c) =>
          loop(c, previousDescription).fold(
            _ => None,
            success => Some(success)
          )

        case ConfigDescriptor.Zip(left, right) =>
          loop(left, previousDescription).either
            .flatMap(
              res1 =>
                loop(right, previousDescription).either.map(
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

        case ConfigDescriptor.Or(left, right) =>
          loop(left, previousDescription).either.flatMap(
            {
              case Right(a) => ZIO.access(_ => Left(a))
              case Left(lerr) =>
                loop(right, previousDescription).either.flatMap(
                  {
                    case Right(b)   => ZIO.access(_ => Right(b))
                    case Left(rerr) => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    loop(configuration, "")
  }
}
