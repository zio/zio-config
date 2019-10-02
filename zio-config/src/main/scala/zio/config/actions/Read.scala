package zio.config.actions

import zio.config.ReadErrors
import zio.config.{ ConfigDescriptor, ConfigReport, ConfigSource, Details, ReadError }
import zio.{ config, Ref, UIO, ZIO }

object Read {
  // Read
  final def read[A](configuration: ConfigDescriptor[A]): ZIO[ConfigSource, ReadErrors, (ConfigReport, A)] = {
    def loop[B](
      configuration: ConfigDescriptor[B],
      report: Ref[ConfigReport],
      previousDescription: String
    ): ZIO[ConfigSource, ReadErrors, (ConfigReport, B)] =
      configuration match {
        case ConfigDescriptor.Succeed(value) => report.get.map(t => (t, value))

        case ConfigDescriptor.Source(path, propertyType) =>
          for {
            value <- config
                      .getConfigValue(path)
                      .mapError(_ => ReadErrors(ReadError(path, ReadError.MissingValue)))
            r <- report
                  .update(_.addDetails(Details(path, value, previousDescription)))
            result <- ZIO.fromEither(
                       propertyType
                         .read(value)
                         .fold(r => Left(ReadErrors(ReadError(path, r))), e => Right((r, e)))
                     )

          } yield result

        case ConfigDescriptor.MapEither(c, f, _) =>
          loop(c, report, previousDescription).flatMap {
            case (r, src) => ZIO.fromEither(f(src)).bimap(err => ReadErrors(err), res => (r, res))
          }

        case ConfigDescriptor.Describe(c, message) =>
          loop(c, report, message)

        case ConfigDescriptor.Optional(c) =>
          report.get.flatMap(
            t =>
              loop(c, report, previousDescription).fold(
                _ => (t, None),
                success => (success._1, Some(success._2))
              )
          )

        case ConfigDescriptor.OnError(c, f) =>
          ZIO.accessM[ConfigSource](
            _ =>
              loop(c, report, previousDescription)
                .foldM(
                  errors => report.get.map(r => (r, f(errors))),
                  UIO(_)
                )
          )

        case ConfigDescriptor.Zip(left, right) =>
          loop(left, report, previousDescription).either
            .flatMap(
              res1 =>
                loop(right, report, previousDescription).either.map(
                  res2 =>
                    (res1, res2) match {
                      case (Right((_, a)), Right((report2, b))) => Right((report2, (a, b)))
                      case (Left(a), Right(_))                  => Left(a)
                      case (Right(_), Left(error))              => Left(error)
                      case (Left(err1), Left(err2))             => Left(ReadErrors.concat(err1, err2))
                    }
                )
            )
            .absolve

        case ConfigDescriptor.Or(left, right) =>
          loop(left, report, previousDescription).either.flatMap(
            {
              case Right((r, a)) => ZIO.access(_ => (r, Left(a)))
              case Left(lerr) =>
                loop(right, report, previousDescription).either.flatMap(
                  {
                    case Right((r, b)) => ZIO.access(_ => (r, Right(b)))
                    case Left(rerr)    => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )
      }

    Ref.make(ConfigReport(Nil)).flatMap(report => loop(configuration, report, ""))
  }
}
