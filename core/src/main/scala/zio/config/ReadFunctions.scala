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
    ): ZIO[Any, ReadErrorsVector[K, V1], ::[Option[B]]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          for {
            value <- source.getConfigValue(paths :+ path)
            result <- foreach(value.value) {
                       case Some(value) =>
                         ZIO.fromEither(
                           propertyType
                             .read(value)
                             .fold(
                               r =>
                                 Left(
                                   singleton(
                                     ReadError
                                       .parseError(paths :+ path, r.value, r.typeInfo): ReadError[Vector[K], V1]
                                   )
                                 ),
                               e => Right(Some(e))
                             )
                         )
                       case None => ZIO.succeed(None: Option[B])
                     }
          } yield result

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s
          loop(config, paths).map(list => {
            val r = list
            val z = ::(r.head, r.tail)
            singleton(Some(z.map(_.toList).flatten))
          })

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, b] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths).flatMap { as =>
            foreach(as)({
              case None => ZIO.succeed(None: Option[b])
              case Some(v) =>
                ZIO
                  .fromEither(f(v))
                  .bimap(
                    err =>
                      singleton(
                        ReadError.fatalError[Vector[K], V1](paths, new RuntimeException(err))
                      ),
                    res => Option(res)
                  )
            })
          }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, paths).fold(
            _ => singleton(Some(value)),
            identity
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case cd: ConfigDescriptor.Optional[K, V, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).either.flatMap({
            case Right(value) =>
              val res: List[Option[B]] = value.map(t => Some(t): Option[B])
              ZIO.succeed(::(res.head, res.tail))
            case Left(_) => ZIO.succeed(singleton(None))
          })

        case r: ConfigDescriptor.Zip[K, V, a, b] @unchecked => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths).either
            res2 <- loop(right, paths).either
            rr <- (res1, res2) match {
                   case (Right(as), Right(bs)) =>
                     foreach(::(as.zip(bs).head, as.zip(bs).tail)) {
                       case (Some(a), Some(b)) => ZIO.succeed(Some((a, b)))
                       case (Some(a), None)    => ZIO.succeed(Some((a, None.asInstanceOf[b])))
                       case (None, Some(b))    => ZIO.succeed(Some((None.asInstanceOf[a], b)))
                       case (None, None)       => ZIO.succeed(Some((None.asInstanceOf[a], None.asInstanceOf[b])))
                     }
                   case (Left(a), Right(_))      => ZIO.fail(a)
                   case (Right(_), Left(error))  => ZIO.fail(error)
                   case (Left(err1), Left(err2)) => ZIO.fail(concat(err1, err2))
                 }
          } yield rr
        }

        case cd: ConfigDescriptor.OrElseEither[K, V, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  ZIO.succeed(
                    ::(
                      a.map(r => r.map(rr => Left(rr): Either[a, b])).head,
                      a.map(r => r.map(rr => Left(rr): Either[a, b])).tail
                    )
                  )

                case Left(lerr) =>
                  loop(right, paths).either.flatMap(
                    {
                      case Right(b) =>
                        ZIO.succeed(
                          ::(
                            b.map(r => r.map(rr => Right(rr): Either[a, b])).head,
                            b.map(r => r.map(rr => Right(rr): Either[a, b])).tail
                          )
                        )
                      case Left(rerr) => ZIO.fail(concat(lerr, rerr))
                    }
                  )
              }
            )

        case ConfigDescriptor.OrElse(left, right) =>
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  ZIO.succeed(a)

                case Left(_) =>
                  loop(right, paths)
              }
            )
      }

    loop(configuration, Vector.empty[K]).map(_.flatMap(_.toList)).map(_.head)

  }
}
