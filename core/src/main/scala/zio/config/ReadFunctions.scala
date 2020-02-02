package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.config.ReadError.ParseError
import zio.config.ReadFunctions.ValueType
import zio.config.ReadFunctions.ValueType.Exists
import zio.config.ReadFunctions.ValueType._

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Any, ::[Either[Exists[Vector[K], B], ValueType[Vector[K], V1]]], ::[B]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          val results = for {
            rawValue <- source.getConfigValue(paths :+ path).either

            result <- ZIO.succeed(rawValue match {
                       case Left(error) =>
                         singleton(Right(ValueType.nonFatalErrorValue[Vector[K], V1](error)))
                       case Right(opt) =>
                         opt match {
                           case Some(values) =>
                             mapCons(withIndex(values.value)) {
                               case (Some(value), _) =>
                                 propertyType
                                   .read(value)
                                   .fold(
                                     r =>
                                       Right(
                                         ValueType.parseErrorValue[Vector[K], V1](
                                           ReadError.ParseError(paths :+ path, r.value, r.typeInfo)
                                         )
                                       ),
                                     e => Left(ValueType.existingValue[Vector[K], B](paths :+ path, e))
                                   )
                               case (None, id) =>
                                 Right(ValueType.nonExistingValue[Vector[K], V1](paths :+ path, Some(id)))
                             }

                           case None => singleton(Right(ValueType.nonExistingValue[Vector[K], V1](paths :+ path, None)))
                         }
                     })

            successOrFailure = if (result.forall(_.isLeft))
              Right(result.collect({ case Left(ValueType.Exists(k, a)) => a }))
            else {

              Left(result)
            }
          } yield successOrFailure

          results.flatMap({
            case Left(value) =>
              ZIO.fail(value)
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

              if (list.isEmpty) errs else singleton(Right(listErrors(::(list.head, list.tail))))
            },
            list => singleton(list)
          )

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths)
            .mapError({ errors =>
              // always duplicate the error side and non-error side.
              mapCons(errors)({
                case Left(ValueType.Exists(k, a)) =>
                  f(a) match {
                    case Left(value) =>
                      Right(
                        ValueType.nonFatalErrorValue[Vector[K], V1](
                          ReadError.Unknown[Vector[K]](k, new RuntimeException(value))
                        )
                      )
                    case Right(value) => Left(ValueType.existingValue[Vector[K], B](k, value))
                  }
                case Right(value) =>
                  Right(value)
              })
            })
            .flatMap { as =>
              foreach(as)(a => {
                ZIO
                  .fromEither(f(a))
                  .bimap(
                    err => {
                      singleton(
                        Right(
                          ValueType.nonFatalErrorValue[Vector[K], V1](
                            ReadError.Unknown[Vector[K]](paths, new RuntimeException(err))
                          )
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
            _ => singleton(value),
            identity
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case cd: ConfigDescriptor.Optional[K, V1, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).either.flatMap({
            case Right(value) =>
              ZIO.succeed(mapCons(value)(t => Some(t): Option[B]))
            case Left(r) =>
              if (hasNonFatalErrors[K, V1, B](r) || hasParseErrors[K, V1, B](r))
                ZIO.fail(r)
              else
                ZIO.succeed({
                  val result = r.map({
                    case Left(ValueType.Exists(k, a)) => Some(a)
                    case _                            => None
                  })

                  if (result.isEmpty) ::(None, Nil) else ::(result.head, result.tail)
                })
          })

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths).either
            res2 <- loop(right, paths).either
            r <- (res1, res2) match {
                  case (Right(as), Right(bs)) =>
                    ZIO.succeed(zipCons(as, bs))

                  case (Left(aa), Right(b)) =>
                    ZIO.fail(
                      mapCons(withIndex(aa))({
                        case (Left((ValueType.Exists(k, a))), id) =>
                          b.lift(id) match {
                            case Some(v) => Left(Exists[Vector[K], (a, b)](k, (a, v)))
                            case None =>
                              Right(ValueType.nonExistingValue[Vector[K], V1](paths, Some(id)))
                          }

                        case (Right(a), _) => Right(a)
                      })
                    )
                  case (Right(aa), Left(bb)) =>
                    ZIO.fail(mapCons(withIndex(bb))({
                      case (Left(ValueType.Exists(k, a)), id) =>
                        aa.lift(id) match {
                          case Some(v) =>
                            Left(ValueType.existingValue[Vector[K], (a, b)](k, (v, a)))
                          case None =>
                            Right(ValueType.nonExistingValue[Vector[K], V1](paths, Some(id)))
                        }

                      case (Right(a), _) => Right(a)
                    }))

                  case (Left(err1), Left(err2)) =>
                    val res = err1
                      .collect({
                        case Right(value1) =>
                          err2.collect({
                            case Right(value2) =>
                              List(value1, value2): List[ValueType[Vector[K], V1]]
                          })
                      })
                      .flatten

                    ZIO.fail(singleton(Right(zipErrors(::(res.flatten.head, res.flatten.tail)))))
                }
          } yield r
        }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  val result = mapCons(a)(r => Left(r): Either[a, b])
                  ZIO.succeed(result)

                case Left(lerr) =>
                  loop(right, paths).either.flatMap({
                    case Right(b) =>
                      val result = mapCons(withIndex(lerr))({
                        case (Left((ValueType.Exists(k, a))), _) =>
                          // If left already exists, use it !
                          Left(ValueType.existingValue[Vector[K], Either[a, b]](k, Left(a)))

                        case (Right(ValueType.NonFatalErrorValue(error)), id) =>
                          Right(ValueType.nonFatalErrorValue(error))

                        // If left is non existing, then try and get the value from right as well, if not both are non existing values
                        case (Right(v), id) =>
                          b.lift(id) match {
                            case Some(value) =>
                              Left(ValueType.existingValue[Vector[K], Either[a, b]](paths, Right(value)))
                            case None =>
                              Right(v)
                          }
                      })

                      if (result.forall(_.isLeft))
                        ZIO.succeed({
                          val r: List[Either[a, b]] = result.collect({ case Left(ValueType.Exists(k, a)) => a })
                          ::(r.head, r.tail)
                        })
                      else {
                        ZIO.fail(result)
                      }

                    case Left(rerr) =>
                      val result = mapCons(withIndex(lerr))({
                        case (Left(ValueType.Exists(k, a)), _) =>
                          Left(ValueType.existingValue(k, Left(a): Either[a, b]))
                        case (Right(someLeftError), id) =>
                          rerr.lift(id) match {
                            case Some(value) =>
                              value match {
                                case Left(ValueType.Exists(k, v)) =>
                                  Left(ValueType.existingValue[Vector[K], Either[a, b]](k, Right(v): Either[a, b]))
                                case Right(someRightError) =>
                                  Right(ValueType.orErrors[Vector[K], V1](someLeftError, someRightError))
                              }

                            case None => Right(someLeftError)
                          }
                      })

                      if (result.forall(_.isLeft))
                        ZIO.succeed({
                          val r: List[Either[a, b]] = result.collect({ case Left(ValueType.Exists(k, a)) => a })
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
                          case (Left(ValueType.Exists(k, a)), _) => Left(ValueType.existingValue[Vector[K], B](k, a))
                          case (Right(leftError), id) =>
                            rerr.lift(id) match {
                              case Some(value) =>
                                value match {
                                  case Left(ValueType.Exists(k, v)) => Left(ValueType.existingValue[Vector[K], B](k, v))
                                  case Right(anyRightError)         => Right(ValueType.orErrors(leftError, anyRightError))
                                }

                              case None => Right(leftError)
                            }
                        })
                        if (result.forall(_.isLeft)) ZIO.succeed({
                          val res = result.collect({ case Left(ValueType.Exists(k, a)) => a })
                          ::(res.head, res.tail)
                        })
                        else {
                          ZIO.fail(result)
                        }
                      }
                  })
            })
      }

    def partial: PartialFunction[Either[Exists[Vector[K], A], ValueType[Vector[K], V]], ReadErrorsVector[K, V]] = {
      case Right(ValueType.NonExisting(paths, index)) =>
        ::(ReadError.missingValue[Vector[K], V](paths, index), Nil)
      case Right(ValueType.ParseErrorValue(error))    => ::(error: ReadError[Vector[K], V], Nil)
      case Right(ValueType.OrErrors(error1, error2))  => concat(partial(Right(error1)), partial(Right(error2)))
      case Right(ValueType.NonFatalErrorValue(error)) => ::(error, Nil)
      case Right(ValueType.ListErrors(errors)) =>
        val result = mapCons(errors)(eachError => partial(Right(eachError)))
        ::(result.flatten.head, result.flatten.tail)
      case Right(ValueType.ZipErrors(errors)) =>
        val result = mapCons(errors)(eachError => partial(Right(eachError)))
        ::(result.flatten.head, result.flatten.tail)
    }

    loop(configuration, Vector.empty[K]).either
      .map({
        case Left(value) =>
          val res = value
            .filterNot(_.isLeft)
            .collect(partial)
          Left(::(res.flatten.head, res.flatten.tail))
        case Right(value) => Right(value)
      })
      .absolve
      .map(_.head)
  }
}

object ReadFunctions {
  // This is just an intermediate structure while forming the logic. Will be replaced by existing ReadError
  sealed trait ValueType[+K, +V]

  object ValueType {

    case class Exists[K, A](path: K, a: A)

    case class NonExisting[K, V](path: K, position: Option[Int]) extends ValueType[K, V]

    case class ParseErrorValue[K, V](error: ParseError[K, V]) extends ValueType[K, V]

    case class NonFatalErrorValue[K, V](error: ReadError.Unknown[K]) extends ValueType[K, V]

    case class OrErrors[K, V](leftErrors: ValueType[K, V], rightErrors: ValueType[K, V]) extends ValueType[K, V]

    case class ListErrors[K, V](errors: ::[ValueType[K, V]]) extends ValueType[K, V]
    case class ZipErrors[K, V](errors: ::[ValueType[K, V]])  extends ValueType[K, V]

    def nonFatalErrorValue[K, V](error: ReadError.Unknown[K]): ValueType[K, V] =
      NonFatalErrorValue(error)

    def parseErrorValue[K, V](error: ParseError[K, V]): ValueType[K, V] =
      ParseErrorValue(error)

    def nonExistingValue[K, V](path: K, position: Option[Int]): ValueType[K, V] =
      NonExisting(path, position)

    def existingValue[K, A](path: K, a: A): Exists[K, A] =
      Exists(path, a)

    def orErrors[K, V](a: ValueType[K, V], b: ValueType[K, V]): ValueType[K, V] =
      OrErrors(a, b)

    def listErrors[K, V](errors: ::[ValueType[K, V]]): ValueType[K, V] =
      ListErrors(errors)

    def zipErrors[K, V](errors: ::[ValueType[K, V]]): ValueType[K, V] =
      ZipErrors(errors)

    def hasNonFatalErrors[K, V1, B](values: ::[Either[Exists[Vector[K], B], ValueType[Vector[K], V1]]]): Boolean =
      values.exists({
        case Left(_) => false
        case Right(value) =>
          value match {
            case NonExisting(_, _)     => false
            case ParseErrorValue(_)    => false
            case NonFatalErrorValue(_) => true
            case OrErrors(leftErrors, rightErrors) =>
              hasNonFatalErrors[K, V1, B](singleton(Right(leftErrors))) || hasNonFatalErrors[K, V1, B](
                singleton(Right(rightErrors))
              )
            case ListErrors(leftErrors) => hasNonFatalErrors[K, V1, B](mapCons(leftErrors)(Right(_)))
            case ZipErrors(leftErrors)  => hasNonFatalErrors[K, V1, B](mapCons(leftErrors)(Right(_)))
          }
      })

    def hasParseErrors[K, V1, B](values: ::[Either[Exists[Vector[K], B], ValueType[Vector[K], V1]]]): Boolean =
      values.exists({
        case Left(_) => false
        case Right(value) =>
          value match {
            case NonExisting(_, _)     => false
            case ParseErrorValue(_)    => true
            case NonFatalErrorValue(_) => false
            case OrErrors(leftErrors, rightErrors) =>
              hasParseErrors[K, V1, B](singleton(Right(leftErrors))) || hasParseErrors[K, V1, B](
                singleton(Right(rightErrors))
              )
            case ListErrors(leftErrors) => hasParseErrors[K, V1, B](mapCons(leftErrors)(Right(_)))
            case ZipErrors(leftErrors)  => hasParseErrors[K, V1, B](mapCons(leftErrors)(Right(_)))

          }
      })
  }
}
