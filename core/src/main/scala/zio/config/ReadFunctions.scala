package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.Ref
import zio.Queue

private[config] trait ReadFunctions {
  // Read
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Cache[K], ReadErrorsVector[K, V1], B] =
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
                _ <- tail.add(paths, result.tail)

              } yield result.head
          )

        case Sequence(config) =>
          loop(config, paths).flatMap(
            v =>
              ZIO
                .environment[Cache[K]]
                .flatMap(
                  ref =>
                    ref
                      .get(paths)
                      .map(ll => {
                        val r = v :: ll
                        r
                      })
                )
          )

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case ConfigDescriptor.XmapEither(c, f, _) =>
          loop(c, paths).flatMap { a =>
            ZIO
              .environment[Cache[K]]
              .flatMap(
                ref =>
                  ref
                    .get(paths)
                    .flatMap(
                      list => {
                        println(s"the list is ${list}")
                        ZIO
                          .foreach(a :: list)(a => {
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
                      }
                    )
                    .flatMap(
                      list =>
                        ZIO.accessM[Cache[K]](
                          e => (if (list.isEmpty) e.r.get else e.add(paths, list.tail)).map(_ => list.head)
                        )
                    )
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

    Cache.make[K].flatMap(queue => loop(configuration, Vector.empty[K]).provide(queue))
  }
}

final case class Cache[K](r: Ref[Map[Vector[K], List[Any]]]) {
  def get(k: Vector[K])               = r.get.map(_.get(k).toList.flatten)
  def add(k: Vector[K], v: List[Any]) = r.update(r => r.updated(k, v))
}

object Cache {
  def make[K] = Ref.make(Map.empty[Vector[K], List[Any]]).map(Cache(_))
}
