package zio.config.actions

import zio.config.{ Config, ConfigReport, ConfigSource, Details, ReadError }
import zio.{ config, Ref, UIO, ZIO }

case class Read[A](run: ZIO[ConfigSource, List[ReadError], (ConfigReport, A)]) {}

object Read {
  // Read
  final def read[A](configuration: Config[A]): Read[A] = {
    def loop[A](
      configuration: Config[A],
      report: Ref[ConfigReport]
    ): ZIO[ConfigSource, List[ReadError], (ConfigReport, A)] =
      configuration match {
        case Config.Source(path, propertyType) =>
          for {
            value <- config
                      .getConfigValue(path)
                      .mapError(_ => List(ReadError(Seq(path), ReadError.MissingValue)))
            r <- report
                  .update(_.addDetails(Details(path, value, propertyType.description)))
            result <- ZIO.fromEither(
                       propertyType
                         .read(value)
                         .fold(r => Left(List(ReadError(Seq(path), r))), e => Right((r, e)))
                     )

          } yield result

        case Config.MapEither(c, f, _) =>
          loop(c, report).flatMap {
            case (r, src) => ZIO.fromEither(f(src)).bimap(tt => List(tt), res => (r, res))
          }

        case Config.OnError(c, f) =>
          ZIO.accessM[ConfigSource](
            _ =>
              loop(c, report).foldM(
                failure =>
                  report.get.flatMap[Any, List[ReadError], (ConfigReport, A)](r => ZIO.succeed((r, f(failure)))),
                UIO(_)
              )
          )

        case Config.Zip(left, right) =>
          loop(left, report).either
            .flatMap(
              res1 =>
                loop(right, report).either.map(
                  res2 =>
                    (res1, res2) match {
                      case (Right((_, a)), Right((report2, b))) => Right((report2, (a, b)))
                      case (Left(a), Right(_))                  => Left(a)
                      case (Right(_), Left(error))              => Left(error)
                      case (Left(err1), Left(err2))             => Left(err1 ++ err2)
                    }
                )
            )
            .absolve

        case Config.Or(left, right) =>
          loop(left, report).either.flatMap(
            {
              case Right((r, a)) => ZIO.access(_ => (r, Left(a)))
              case Left(lerr) =>
                loop(right, report).either.flatMap(
                  {
                    case Right((r, b)) => ZIO.access(_ => (r, Right(b)))
                    case Left(rerr)    => ZIO.fail(lerr ++ rerr)
                  }
                )
            }
          )

        case Config.Xmap(c, from, _) =>
          loop(c, report).map(t => (t._1, from(t._2)))
      }

    Read(Ref.make(ConfigReport(Nil)).flatMap(report => loop(configuration, report)))
  }
}
