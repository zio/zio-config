package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.config.ReadFunctions.{ Exists, ValueType }
import zio.config.ReadFunctions.ValueType._

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Any, ::[Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]], ::[::[B]]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          val results
            : ZIO[Any, Nothing, Either[::[Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]], List[::[B]]]] =
            for {
              result <- source
                         .getConfigValue(paths :+ path)
                         .either
                         .map({
                           case Left(error) =>
                             singleton(
                               Right(error: ReadError[Vector[K], V1]): Either[
                                 Exists[Vector[K], ::[B]],
                                 ReadError[Vector[K], V1]
                               ]
                             )
                           case Right(opt) =>
                             opt match {
                               case Some(values) =>
                                 mapCons(withIndex(values.value)) {
                                   case (Some(values), _) =>
                                     seqEitherCons(
                                       mapCons(values)(
                                         value =>
                                           propertyType
                                             .read(value)
                                       )
                                     ).fold(
                                       r =>
                                         Right(
                                           ReadError.ParseError(paths :+ path, r.value, r.typeInfo)
                                         ): Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]],
                                       e =>
                                         Left(ValueType.existingValue[Vector[K], ::[B]](paths :+ path, e)): Either[
                                           Exists[Vector[K], ::[B]],
                                           ReadError[Vector[K], V1]
                                         ]
                                     )
                                   case (None, id) =>
                                     Right(ReadError.MissingValue[Vector[K]](paths :+ path, Some(id))): Either[
                                       Exists[Vector[K], ::[B]],
                                       ReadError[Vector[K], V1]
                                     ]
                                 }

                               case None =>
                                 singleton(
                                   Right(ReadError.MissingValue[Vector[K]](paths :+ path, None)): Either[Exists[Vector[
                                     K
                                   ], ::[B]], ReadError[Vector[K], V1]]
                                 )
                             }
                         })

              successOrFailure = if (result.forall(_.isLeft))
                Right(result.collect({ case Left(Exists(_, a)) => a }))
              else {
                Left(result)
              }
            } yield successOrFailure

          results.flatMap({
            case Left(value) =>
              ZIO.fail[::[Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]]](
                value: ::[Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]]
              )
            case Right(value) => ZIO.succeed(::(value.head, value.tail))
          })

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s
          loop(config, paths).bimap(
            errs => {
              val list =
                errs.collect({
                  case Right(value) => value
                })

              if (list.isEmpty) errs else singleton(Right(ReadError.AndErrors(::(list.head, list.tail))))
            },
            list => singleton(list).asInstanceOf[::[::[B]]]
          )

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths)
            .mapError({ errors =>
              mapCons(errors)({
                case Left(Exists(k, a)) =>
                  seqEitherCons(mapCons(a)(f)) match {
                    case Left(value) =>
                      Right(
                        ReadError.Unknown[Vector[K]](k, new RuntimeException(value))
                      )
                    case Right(value) => Left(ValueType.existingValue[Vector[K], ::[B]](k, value))
                  }
                case Right(value) =>
                  Right(value)
              })
            })
            .flatMap { as =>
              foreach(as)(a => {
                ZIO
                  .fromEither(seqEitherCons(mapCons(a)(f)))
                  .bimap(
                    err => {
                      singleton(
                        Right(
                          ReadError.Unknown[Vector[K]](paths, new RuntimeException(err))
                        )
                      )
                    },
                    res => res
                  )
              })
            }

        // No need to add report on the default value.
        case ConfigDescriptor.Default(c, value) =>
          loop(c, paths).fold(
            _ => singleton(singleton(value)),
            identity
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case cd: ConfigDescriptor.Optional[K, V1, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).either.flatMap({
            case Right(value) =>
              ZIO.succeed(mapCons(value)(t => mapCons(t)(tt => Some(tt): Option[B])))
            case Left(r) =>
              if (hasNonFatalErrors[K, V1, ::[B]](r) || hasParseErrors[K, V1, ::[B]](r))
                ZIO.fail(r)
              else
                ZIO.succeed({
                  val result: List[::[Option[B]]] = r.map({
                    case Left(Exists(_, a)) => mapCons(a)(aa => Some(aa): Option[B])
                    case _                  => singleton(None: Option[B])
                  })

                  if (result.isEmpty) ::(singleton(None: Option[B]), Nil) else ::(result.head, result.tail)
                })
          })

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths).either
            res2 <- loop(right, paths).either
            r <- (res1, res2) match {
                  case (Right(as), Right(bs)) =>
                    val result1 = as.flatten.zip(bs.flatten)
                    val result  = singleton(::(result1.head, result1.tail))

                    ZIO.succeed(::(result.head, result.tail))

                  case (Left(aa), Right(b)) =>
                    ZIO.fail(
                      mapCons(withIndex(aa))({
                        case (Left(Exists(k, a)), id) =>
                          b.lift(id) match {
                            case Some(v) =>
                              Left(Exists[Vector[K], ::[(a, b)]](k, {
                                val r = (a.flatMap(aa => v.map(vv => (aa, vv)))); ::(r.head, r.tail)
                              })): Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]
                            case None =>
                              Right(ReadError.MissingValue[Vector[K]](paths, Some(id))): Either[Exists[Vector[K], ::[
                                B
                              ]], ReadError[Vector[K], V1]]
                          }

                        case (Right(a), _) => Right(a): Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]
                      })
                    )
                  case (Right(aa), Left(bb)) =>
                    ZIO.fail(mapCons(withIndex(bb))({
                      case (Left(Exists(k, a)), id) =>
                        aa.lift(id) match {
                          case Some(v) =>
                            val result = v.zip(a)
                            Left(ValueType.existingValue[Vector[K], ::[(a, b)]](k, ::(result.head, result.tail))): Either[
                              Exists[Vector[K], ::[B]],
                              ReadError[Vector[K], V1]
                            ]
                          case None =>
                            Right(ReadError.MissingValue[Vector[K]](paths, Some(id))): Either[
                              Exists[Vector[K], ::[B]],
                              ReadError[Vector[K], V1]
                            ]
                        }

                      case (Right(a), _) => Right(a): Either[Exists[Vector[K], ::[B]], ReadError[Vector[K], V1]]
                    }))

                  case (Left(err1), Left(err2)) =>
                    val res = err1
                      .collect({
                        case Right(value1) =>
                          err2.collect({
                            case Right(value2) =>
                              List(value1, value2): List[ReadError[Vector[K], V1]]
                          })
                      })
                      .flatten

                    ZIO.fail(
                      singleton(
                        Right(ReadError.AndErrors(::(res.flatten.head, res.flatten.tail))): Either[Exists[Vector[K], ::[
                          B
                        ]], ReadError[Vector[K], V1]]
                      )
                    )
                }
          } yield r
        }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  val result = mapCons(a)(r => mapCons(r)(rr => Left(rr): Either[a, b]))
                  ZIO.succeed(result)

                case Left(lerr) =>
                  loop(right, paths).either.flatMap({
                    case Right(b) =>
                      val result = mapCons(withIndex(lerr))({
                        case (Left((Exists(k, a))), _) =>
                          // If left already exists, use it !
                          Left(ValueType.existingValue[Vector[K], ::[Either[a, b]]](k, mapCons(a)(aa => Left(aa))))

                        // If left is non existing, then try and get the value from right as well, if not both are non existing values
                        case (Right(v), id) =>
                          b.lift(id) match {
                            case Some(value) =>
                              Left(
                                ValueType
                                  .existingValue[Vector[K], ::[Either[a, b]]](paths, mapCons(value)(aa => Right(aa)))
                              )
                            case None =>
                              Right(v)
                          }
                      })

                      if (result.forall(_.isLeft))
                        ZIO.succeed({
                          val r: List[::[Either[a, b]]] = result.collect({ case Left(Exists(_, a)) => a })
                          ::(r.head, r.tail)
                        })
                      else {
                        ZIO.fail(result)
                      }

                    case Left(rerr) =>
                      val result = mapCons(withIndex(lerr))({
                        case (Left(Exists(k, a)), _) =>
                          Left(ValueType.existingValue(k, mapCons(a)(aa => Left(aa): Either[a, b])))
                        case (Right(someLeftError), id) =>
                          rerr.lift(id) match {
                            case Some(value) =>
                              value match {
                                case Left(Exists(k, v)) =>
                                  Left(
                                    ValueType.existingValue[Vector[K], ::[Either[a, b]]](
                                      k,
                                      mapCons(v)(aa => Right(aa): Either[a, b])
                                    )
                                  )
                                case Right(someRightError) =>
                                  Right(ReadError.OrErrors[Vector[K], V1](someLeftError, someRightError))
                              }

                            case None => Right(someLeftError)
                          }
                      })

                      if (result.forall(_.isLeft))
                        ZIO.succeed({
                          val r: List[::[Either[a, b]]] = result.collect({ case Left(Exists(_, a)) => a })
                          ::(r.head, r.tail)
                        })
                      else {
                        ZIO.fail(result)
                      }

                  })
              }
            )

        case ConfigDescriptor.OrElse(left, right) =>
          loop(left, paths).either
            .flatMap({
              case Right(a) =>
                ZIO.succeed(a)

              case Left(lerr) =>
                if (hasNonFatalErrors(lerr))
                  ZIO.fail(lerr)
                else
                  loop(right, paths).either.flatMap({
                    case Right(b) =>
                      ZIO.succeed(b)
                    case Left(rerr) =>
                      if (hasNonFatalErrors(rerr))
                        ZIO.fail(
                          concat(lerr, rerr)
                        )
                      else {
                        val result = mapCons(withIndex(lerr))({
                          case (Left(Exists(k, a)), _) => Left(ValueType.existingValue[Vector[K], ::[B]](k, a))
                          case (Right(leftError), id) =>
                            rerr.lift(id) match {
                              case Some(value) =>
                                value match {
                                  case Left(Exists(k, v))   => Left(ValueType.existingValue[Vector[K], ::[B]](k, v))
                                  case Right(anyRightError) => Right(ReadError.OrErrors(leftError, anyRightError))
                                }

                              case None => Right(leftError)
                            }
                        })
                        if (result.forall(_.isLeft)) ZIO.succeed({
                          val res = result.collect({ case Left(Exists(_, a)) => a })
                          ::(res.head, res.tail)
                        })
                        else {
                          ZIO.fail(result)
                        }
                      }
                  })
            })
      }

    loop(configuration, Vector.empty[K]).either
      .map({
        case Left(value) =>
          val res = value
            .collect({
              case Right(value) => value
            })
          Left(::(res.head, res.tail))
        case Right(value) => Right(value)
      })
      .absolve
      .map(_.head.head)
  }
}

object ReadFunctions {

  final case class Exists[K, A](path: K, a: A)

  // This is just an intermediate structure while forming the logic. Will be replaced by existing ReadError

  object ValueType {
    def existingValue[K, V](path: K, a: V): Exists[K, V] = Exists(path, a)

    final def hasNonFatalErrors[K, V1, B](values: ::[Either[Exists[Vector[K], B], ReadError[Vector[K], V1]]]): Boolean =
      values.exists({
        case Left(_) => false
        case Right(value) =>
          value match {
            case ReadError.MissingValue(_, _)  => false
            case ReadError.ParseError(_, _, _) => false
            case ReadError.Unknown(_, _)       => true
            case ReadError.OrErrors(leftErrors, rightErrors) =>
              hasNonFatalErrors[K, V1, B](singleton(Right(leftErrors))) || hasNonFatalErrors[K, V1, B](
                singleton(Right(rightErrors))
              )
            case ReadError.AndErrors(leftErrors) => hasNonFatalErrors[K, V1, B](mapCons(leftErrors)(Right(_)))
          }
      })

    final def hasParseErrors[K, V1, B](values: ::[Either[Exists[Vector[K], B], ReadError[Vector[K], V1]]]): Boolean =
      values.exists({
        case Left(_) => false
        case Right(value) =>
          value match {
            case ReadError.MissingValue(_, _)  => false
            case ReadError.ParseError(_, _, _) => true
            case ReadError.Unknown(_, _)       => false
            case ReadError.OrErrors(leftErrors, rightErrors) =>
              hasParseErrors[K, V1, B](singleton(Right(leftErrors))) || hasParseErrors[K, V1, B](
                singleton(Right(rightErrors))
              )
            case ReadError.AndErrors(leftErrors) => hasParseErrors[K, V1, B](mapCons(leftErrors)(Right(_)))
          }
      })
  }
}
