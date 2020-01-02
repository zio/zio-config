package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence

private[config] trait ReadFunctions {
  // Read
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Any, ReadErrorsVector[K, V1], ::[B]] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.succeed(singleton(None))

        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          for {
            value <- source.getConfigValue(paths :+ path)
            result <- ZIO.foreach(value.value) { eachValue =>
                       ZIO.fromEither(
                         propertyType
                           .read(eachValue)
                           .fold(
                             r =>
                               Left(
                                 singleton(
                                   ReadError
                                     .ParseError(paths :+ path, r.value, r.typeInfo): ReadError[Vector[K], V1]
                                 )
                               ),
                             e => Right(e)
                           )
                       )
                     }
          } yield ::(result.head, result.tail)

        case s: Sequence[K, V1, B] =>
          val Sequence(config) = s
          loop(config, paths).map(list => singleton(list))

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths).flatMap { as =>
            ZIO
              .foreach(as)(a => {
                println(a)
                println(scala.util.Try {
                  f(a)
                })

                ZIO
                  .fromEither(f(a))
                  .bimap(
                    err =>
                      singleton(
                        ReadError.FatalError(paths, new RuntimeException(err))
                      ),
                    res => res
                  )
              })
              .map(list => ::(list.head, list.tail))
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, paths).fold(
            _ => singleton(value),
            identity
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case ConfigDescriptor.Optional(c) =>
          loop(c, paths).option.map(singleton)

        case r: ConfigDescriptor.Zip[K, V, a, B] => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths).either
            res2 <- loop(right, paths).either
            r <- ZIO
                  .fromEither((res1, res2) match {
                    case (Right(as), Right(bs)) =>
                      Right(as.zip(bs))
                    case (Left(a), Right(_))      => Left(a)
                    case (Right(_), Left(error))  => Left(error)
                    case (Left(err1), Left(err2)) => Left(concat(err1, err2))
                  })
                  .map(list => ::(list.head, list.tail))
          } yield r
        }

        case cd: ConfigDescriptor.OrElseEither[K, V, a, b] =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  ZIO.succeed(::(a.map(r => Left(r): Either[a, b]).head, a.map(r => Left(r): Either[a, b])))

                case Left(lerr) =>
                  val result = loop(right, paths).either.flatMap(
                    {
                      case Right(b) =>
                        ZIO.succeed(::(b.map(r => Right(r): Either[a, b]).head, b.map(r => Right(r): Either[a, b])))
                      case Left(rerr) => ZIO.fail(concat(lerr, rerr))
                    }
                  )

                  result
              }
            )
      }

    loop(configuration, Vector.empty[K]).map(_.head)
  }
}
