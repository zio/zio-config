package zio

import java.net.URI

package object config extends Sources {
  def int(path: String): Config[Int]       = Config.Source(path, PropertyType.IntType)
  def double(path: String): Config[Double] = Config.Source(path, PropertyType.DoubleType)
  def string(path: String): Config[String] = Config.Source(path, PropertyType.StringType)
  def long(path: String): Config[Long]     = Config.Source(path, PropertyType.LongType)
  def short(path: String): Config[Short]   = Config.Source(path, PropertyType.ShortType)
  def uri(path: String): Config[URI]       = Config.Source(path, PropertyType.UriType)

  def opt[A](config: => Config[A]): Config[Option[A]] =
    config
      .mapEither[Option[A]](a => Right(Some(a)))({
        case Some(value) => Right(value)
        case None        => Left(WriteError("Error: Cannot write a none value", None))
      })
      .onError(_ => None)

  def read[A](config: => Config[A]): ZIO[ConfigSource, List[ReadError], (ConfigReport, A)] = {
    def loop[B](
      configuration: Config[B],
      report: Ref[ConfigReport]
    ): ZIO[ConfigSource, ReadErrors, (ConfigReport, B)] =
      configuration match {
        case Config.Source(path, propertyType) =>
          for {
            value <- getConfigValue(path).mapError(_ => ReadErrors(ReadError(Seq(path), ReadError.MissingValue)))
            r     <- report.update(_.addDetails(Details(path, value, propertyType.description)))
            result <- ZIO.fromEither(
                       propertyType
                         .read(value)
                         .fold(r => Left(ReadErrors(ReadError(Seq(path), r))), e => Right((r, e)))
                     )

          } yield result
        case Config.MapEither(c, f, _) =>
          loop(c, report).flatMap {
            case (r, src) => ZIO.fromEither(f(src)).bimap(err => ReadErrors(err), res => (r, res))
          }
        case Config.OnError(c, f) =>
          ZIO.accessM[ConfigSource](
            _ =>
              loop(c, report)
                .foldM(
                  errors => report.get.map(r => (r, f(errors))),
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
                      case (Left(err1), Left(err2))             => Left(ReadErrors.concat(err1, err2))
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
                    case Left(rerr)    => ZIO.fail(ReadErrors.concat(lerr, rerr))
                  }
                )
            }
          )

        case Config.Xmap(c, from, _) =>
          loop(c, report).map(t => (t._1, from(t._2)))
      }

    Ref.make(ConfigReport(Nil)).flatMap(report => loop(config, report))
  }

  def write[A](config: => Config[A]): ZIO[A, WriteError, Map[String, String]] =
    config match {
      case Config.Source(path, propertyType) =>
        ZIO.access { aa =>
          Map(path -> propertyType.write(aa))
        }
      case Config.Xmap(c, _, to) =>
        ZIO.accessM(b => write(c).provide(to(b)))
      case Config.OnError(c, _) =>
        ZIO.accessM(
          b =>
            write(c)
              .provide(b)
              .fold(
                _ => Map.empty,
                success => success
              )
        )
      case Config.MapEither(c, _, to) =>
        ZIO.accessM { b =>
          to(b) match {
            case Right(before) => write(c).provide(before)
            case Left(error)   => ZIO.fail(error)
          }
        }
      case Config.Or(left, right) =>
        ZIO.accessM(env => env.fold(a => write(left).provide(a), b => write(right).provide(b)))
      case Config.Zip(config1, config2) =>
        ZIO.accessM(
          env =>
            write(config1)
              .provide(env._1)
              .flatMap(value => write(config2).provide(env._2).map(kv => value ++ kv))
        )
    }

  def report[A](config: => Config[A]): ZIO[ConfigSource, List[ReadError], ConfigReport] =
    read(config).map(_._1)

  def getConfigValue(path: String): ZIO[ConfigSource, Unit, String] = ZIO.accessM(_.configService.getConfigValue(path))

  ////

  type ReadErrors = ::[ReadError]

  object ReadErrors {
    def apply(a: ReadError, as: ReadError*): ReadErrors =
      ::(a, as.toList)

    def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
      ::(l.head, l.tail ++ r)
  }
}
