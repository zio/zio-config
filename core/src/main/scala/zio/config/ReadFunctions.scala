package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.Ref

private[config] trait ReadFunctions {
  // Read
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Ref[List[Any]], ReadErrorsVector[K, V1], B] =
      configuration match {
        case ConfigDescriptor.Empty() => ZIO.succeed(None)

        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          ZIO.accessM(
            tail =>
              for {
                value <- source.getConfigValue(paths :+ path)
                result <- ZIO.foreach(value.value.toList) { eachValue =>
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
                _ <- tail.update(_ ++ result.tail)

              } yield result.head
          )

        case Sequence(config) =>
          loop(config, paths).flatMap(
            v =>
              ZIO
                .environment[Ref[List[Any]]]
                .flatMap(
                  ref =>
                    ref.get.map(ll => {
                      v :: ll
                    })
                )
          )

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, paths).flatMap { a =>
            ZIO
              .fromEither(f(a))
              .bimap(
                err =>
                  singleton(
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
                      case (Left(err1), Left(err2)) => Left(concat(err1, err2))
                    }
                )
            )
            .absolve

        case ConfigDescriptor.OrElseEither(left, right) =>
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) => ZIO.succeed(Left(a))
                case Left(lerr) =>
                  loop(right, paths).either.flatMap(
                    {
                      case Right(b)   => ZIO.succeed(Right(b))
                      case Left(rerr) => ZIO.fail(concat(lerr, rerr))
                    }
                  )
              }
            )
      }

    Ref.make(Nil: List[Any]).flatMap(l => loop(configuration, Vector.empty[K]).provide(l))
  }
}
