package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.config.ReadError.ParseError
import zio.config.ReadFunctions.ValueType

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K, V], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Any, ::[ValueType[Vector[K], V1, B]], ::[B]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          val results = for {
            rawValue <- source.getConfigValue(paths :+ path).either

            result <- ZIO.succeed(rawValue match {
                       case Left(error) => List(ValueType.nonFatalErrorValue[Vector[K], V1, B](error))
                       case Right(opt) =>
                         opt match {
                           case Some(values) =>
                             values.value.zipWithIndex.map {
                               case (Some(value), id) =>
                                 propertyType
                                   .read(value)
                                   .fold(
                                     r =>
                                       ValueType.parseErrorValue[Vector[K], V1, B](
                                         ReadError.ParseError(paths :+ path, r.value, r.typeInfo)
                                       ),
                                     e => ValueType.existingValue[Vector[K], V1, B](e)
                                   )
                               case (None, id) => ValueType.nonExistingValue[Vector[K], V1, B](paths :+ path, Some(id))
                             }

                           case None => List(ValueType.nonExistingValue[Vector[K], V1, B](paths :+ path, None))
                         }
                     })

            successOrFailure = if (result.forall(_.isSuccess))
              Right(result.collect({ case ValueType.Exists(a) => a }))
            else
              Left(::(result.head, result.tail))
          } yield successOrFailure

          results.flatMap({
            case Left(value)  => ZIO.fail(value)
            case Right(value) => ZIO.succeed(::(value.head, value.tail))
          })

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s
          loop(config, paths).map(list => singleton(list))

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths)
            .mapError({ errors =>
              val result = errors.map({
                case ValueType.Exists(a) =>
                  f(a) match {
                    case Left(value) =>
                      ValueType.nonFatalErrorValue[Vector[K], V1, B](
                        ReadError.Unknown[Vector[K]](paths, new RuntimeException(value))
                      )
                    case Right(value) => ValueType.existingValue[Vector[K], V1, B](value)
                  }
                case ValueType.NonExisting(path, position) =>
                  ValueType.nonExistingValue[Vector[K], V1, B](path, position)
                case ValueType.ParseErrorValue(error)    => ValueType.parseErrorValue[Vector[K], V1, B](error)
                case ValueType.NonFatalErrorValue(error) => ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
              })
              ::(result.head, result.tail)
            })
            .flatMap { as =>
              foreach(as)(a => {
                ZIO
                  .fromEither(f(a))
                  .bimap(
                    err =>
                      singleton(
                        ValueType.nonFatalErrorValue[Vector[K], V1, B](
                          ReadError.Unknown[Vector[K]](paths, new RuntimeException(err))
                        )
                      ),
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

        case cd: ConfigDescriptor.Optional[K, V, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).either.flatMap({
            case Right(value) =>
              val res: List[Option[B]] = value.map(t => Some(t): Option[B])
              ZIO.succeed(::(res.head, res.tail))
            case Left(r) =>
              if (r.exists(_.isNonFatalError) || r.exists(_.isParseError))
                ZIO.fail(r)
              else
                ZIO.succeed({
                  val result = r.collect({
                    case ValueType.Exists(a)         => Some(a)
                    case ValueType.NonExisting(_, _) => None
                  })
                  ::(result.head, result.tail)
                })
          })

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths).either
            res2 <- loop(right, paths).either
            r <- (res1, res2) match {
                  case (Right(as), Right(bs)) =>
                    ZIO.succeed({
                      val res = as.zip(bs)
                      ::(res.head, res.tail)
                    })
                  case (Left(aa), Right(b)) =>
                    val res = aa
                      .zip(b)
                      .map({
                        case (value, value1) =>
                          value match {
                            case ValueType.Exists(a)         => ValueType.existingValue[Vector[K], V1, (a, b)]((a, value1))
                            case ValueType.NonExisting(k, v) => ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v)
                            case ValueType.ParseErrorValue(error) =>
                              ValueType.parseErrorValue[Vector[K], V1, (a, b)](error)
                            case ValueType.NonFatalErrorValue(error) =>
                              ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error)
                          }
                      })
                    ZIO.fail(::(res.head, res.tail))

                  case (Right(aa), Left(bb)) =>
                    val res = aa
                      .zip(bb)
                      .map({
                        case (value1, value) =>
                          value match {
                            case ValueType.Exists(a)         => ValueType.existingValue[Vector[K], V1, (a, b)]((value1, a))
                            case ValueType.NonExisting(k, v) => ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v)
                            case ValueType.ParseErrorValue(error) =>
                              ValueType.parseErrorValue[Vector[K], V1, (a, b)](error)
                            case ValueType.NonFatalErrorValue(error) =>
                              ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error)
                          }
                      })
                    ZIO.fail(::(res.head, res.tail))
                  case (Left(err1), Left(err2)) =>
                    val res = err1
                      .zip(err2)
                      .flatMap({
                        case (value, value1) =>
                         (value, value1) match {
                            case (ValueType.Exists(v1), ValueType.Exists(v2)) =>
                              List(ValueType.existingValue[Vector[K], V1, (a, b)]((v1, v2)))
                            case (ValueType.Exists(_), ValueType.NonExisting(k, v)) =>
                              List(ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v))
                            case (ValueType.Exists(_), ValueType.NonFatalErrorValue(error)) =>
                              List(ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error))
                            case (ValueType.Exists(_), ValueType.ParseErrorValue(error)) =>
                              List(ValueType.parseErrorValue[Vector[K], V1, (a, b)](error))

                            case (ValueType.NonExisting(k, v), ValueType.Exists(_)) =>
                              List(ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v))
                            case (ValueType.NonExisting(k, v), ValueType.NonExisting(k2, v2)) =>
                              List(ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v), ValueType.nonExistingValue[Vector[K], V1, (a, b)](k2, v2))
                            case (ValueType.NonExisting(k, v), ValueType.ParseErrorValue(error)) =>
                              List(ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v), ValueType.parseErrorValue[Vector[K], V1, (a, b)](error))
                            case (ValueType.NonExisting(k, v), ValueType.NonFatalErrorValue(error)) =>
                              List(ValueType.nonExistingValue[Vector[K], V1, (a, b)](k, v), ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error))

                            case (ValueType.ParseErrorValue(error), ValueType.Exists(_)) =>
                              List(ValueType.parseErrorValue[Vector[K], V1, (a, b)](error))
                            case (ValueType.ParseErrorValue(error), ValueType.NonExisting(k2, v2)) =>
                              List(ValueType.parseErrorValue[Vector[K], V1, (a, b)](error), ValueType.nonExistingValue[Vector[K], V1, (a, b)](k2, v2))
                            case (ValueType.ParseErrorValue(error), ValueType.ParseErrorValue(error2)) =>
                              List(ValueType.parseErrorValue[Vector[K], V1, (a, b)](error), ValueType.parseErrorValue[Vector[K], V1, (a, b)](error2))
                            case (ValueType.ParseErrorValue(error), ValueType.NonFatalErrorValue(error2)) =>
                              List(ValueType.parseErrorValue[Vector[K], V1, (a, b)](error), ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error2))

                            case (ValueType.NonFatalErrorValue(error), _) =>
                              List(ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error))
                          }
                      })
                    ZIO.fail(::(res.head, res.tail))
                }
          } yield r
        }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                case Right(a) =>
                  ZIO.succeed(::(a.map(r => Left(r)).head, a.map(r => Left(r)).tail)): ZIO[Any, ::[
                    ValueType[Vector[K], V1, Either[a, b]]
                  ], ::[Either[a, b]]]

                case Left(lerr) =>
                  if (lerr.exists(_.isNonFatalError)) {
                    val result = lerr
                      .filterNot(_.isSuccess)
                      .collect({
                        case ValueType.NonExisting(path, position) =>
                          ValueType.nonExistingValue[Vector[K], V1, B](path, position)
                        case ValueType.ParseErrorValue(error) => ValueType.parseErrorValue[Vector[K], V1, B](error)
                        case ValueType.NonFatalErrorValue(error) =>
                          ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
                      })
                    ZIO.fail(::(result.head, result.tail))
                  } else
                    loop(right, paths).either.flatMap({
                      case Right(b) =>
                        val result = lerr.zipWithIndex.map({
                          case (ValueType.Exists(a), id) =>
                            ValueType.existingValue[Vector[K], V1, Either[a, b]](Left(a))
                          case (ValueType.NonExisting(path, position), id) =>
                            b.lift(id) match {
                              case Some(value) => ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(value))
                              case None        => ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](path, position)
                            }
                          case (ValueType.ParseErrorValue(error), id) =>
                            b.lift(id) match {
                              case Some(value) => ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(value))
                              case None        => ValueType.parseErrorValue[Vector[K], V1, Either[a, b]](error)
                            }
                          case (ValueType.NonFatalErrorValue(error), id) =>
                            ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)
                        })

                        ZIO.succeed({
                          val r = result.collect({ case ValueType.Exists(a) => a })
                          ::(r.head, r.tail)
                        })

                      case Left(rerr) =>
                        if (rerr.exists(_.isNonFatalError))
                          ZIO.fail({
                            val result1 = rerr
                              .filterNot(_.isSuccess)
                              .collect({
                                case ValueType.NonExisting(path, position) =>
                                  ValueType.nonExistingValue[Vector[K], V1, B](path, position)
                                case ValueType.ParseErrorValue(error) =>
                                  ValueType.parseErrorValue[Vector[K], V1, B](error)
                                case ValueType.NonFatalErrorValue(error) =>
                                  ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
                              })

                            val result2 = lerr
                              .filterNot(_.isSuccess)
                              .collect({
                                case ValueType.NonExisting(path, position) =>
                                  ValueType.nonExistingValue[Vector[K], V1, B](path, position)
                                case ValueType.ParseErrorValue(error) =>
                                  ValueType.parseErrorValue[Vector[K], V1, B](error)
                                case ValueType.NonFatalErrorValue(error) =>
                                  ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
                              })

                            val results = result1 ++ result2
                            ::(results.head, results.tail)
                          })
                        else {
                          val result = lerr.zipWithIndex.map({
                            case (ValueType.Exists(a), _) =>
                              ValueType.existingValue[Vector[K], V1, Either[a, b]](Left(a): Either[a, b])
                            case (ValueType.NonExisting(k, v), id) =>
                              rerr.lift(id) match {
                                case Some(value) =>
                                  value match {
                                    case ValueType.Exists(v) =>
                                      ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(v): Either[a, b])
                                    case ValueType.NonExisting(k, v) =>
                                      ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](k, v)
                                    case ValueType.ParseErrorValue(error) =>
                                      ValueType.parseErrorValue[Vector[K], V1, Either[a, b]](error)
                                    case ValueType.NonFatalErrorValue(error) =>
                                      ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)
                                  }

                                case None => ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](paths, None)
                              }
                            case (ValueType.ParseErrorValue(ttt), id) =>
                              rerr.lift(id) match {
                                case Some(value) =>
                                  value match {
                                    case ValueType.Exists(v) =>
                                      ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(v): Either[a, b])
                                    case ValueType.NonExisting(k, v) =>
                                      ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](k, v)
                                    case ValueType.ParseErrorValue(error) =>
                                      ValueType.parseErrorValue[Vector[K], V1, Either[a, b]](error)
                                    case ValueType.NonFatalErrorValue(error) =>
                                      ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)
                                  }
                                case None => ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](paths, None)
                              }

                            case (ValueType.NonFatalErrorValue(error), _) =>
                              ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)
                          })

                          if (result.forall(_.isSuccess))
                            ZIO.succeed({
                              val r: List[Either[a, b]] = result.collect({ case ValueType.Exists(a) => a })
                              ::(r.head, r.tail)
                            })
                          else {
                            ZIO.fail(::(result.head, result.tail))
                          }
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
                if (lerr.exists(_.isNonFatalError))
                  ZIO.fail(lerr)
                else
                  loop(right, paths).either.flatMap({
                    case Right(b) =>
                      ZIO.succeed(::(b.head, b.tail))
                    case Left(rerr) =>
                      if (rerr.exists(_.isNonFatalError))
                        ZIO.fail(
                          concat(lerr, rerr)
                        )
                      else {
                        val result = lerr.zipWithIndex.map({
                          case (ValueType.Exists(a), _) => ValueType.existingValue[Vector[K], V1, B](a)
                          case (ValueType.NonExisting(_, _), id) =>
                            rerr.lift(id) match {
                              case Some(value) =>
                                value match {
                                  case ValueType.Exists(v)         => ValueType.existingValue[Vector[K], V1, B](v)
                                  case ValueType.NonExisting(k, v) => ValueType.nonExistingValue[Vector[K], V1, B](k, v)
                                  case ValueType.ParseErrorValue(error) =>
                                    ValueType.parseErrorValue[Vector[K], V1, B](error)
                                  case ValueType.NonFatalErrorValue(error) =>
                                    ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
                                }

                              case None => ValueType.nonExistingValue[Vector[K], V1, B](paths, None)
                            }
                          case (ValueType.ParseErrorValue(a), _) => ValueType.parseErrorValue[Vector[K], V1, B](a)
                          case (ValueType.NonFatalErrorValue(error), _) =>
                            ValueType.nonFatalErrorValue[Vector[K], V1, B](error)
                        })
                        if (result.forall(_.isSuccess)) ZIO.succeed({
                          val res = result.collect({ case ValueType.Exists(a) => a })
                          ::(res.head, res.tail)
                        })
                        else {
                          ZIO.fail(::(result.head, result.tail))
                        }
                      }
                  })
            })
      }

    loop(configuration, Vector.empty[K]).either
      .map({
        case Left(value) =>
          val res = value
            .filterNot(_.isSuccess)
            .collect({
              case ValueType.NonExisting(paths, index) => ReadError.missingValue[Vector[K], V](paths, index)
              case ValueType.ParseErrorValue(error)    => error
              case ValueType.NonFatalErrorValue(error) => error
            })
          Left(::(res.head, res.tail))
        case Right(value) => Right(value)
      })
      .absolve
      .map(_.head)
  }
}

object ReadFunctions {
  sealed trait ValueType[K, V, A] { self =>
    def isSuccess: Boolean = self match {
      case ValueType.Exists(_) => true
      case _                   => false
    }

    def isParseError: Boolean = self match {
      case ValueType.ParseErrorValue(_) => true
      case _                            => false
    }

    def isNonFatalError: Boolean = self match {
      case ValueType.NonFatalErrorValue(_) => true
      case _                               => false
    }
  }

  object ValueType {

    case class Exists[K, V, A](a: A) extends ValueType[K, V, A]

    case class NonExisting[K, V, A](path: K, position: Option[Int]) extends ValueType[K, V, A]

    case class ParseErrorValue[K, V, A](error: ParseError[K, V]) extends ValueType[K, V, A]

    case class NonFatalErrorValue[K, V, A](error: ReadError.Unknown[K]) extends ValueType[K, V, A]

    def nonFatalErrorValue[K, V, A](error: ReadError.Unknown[K]): ValueType[K, V, A] =
      NonFatalErrorValue(error)

    def parseErrorValue[K, V, A](error: ParseError[K, V]): ValueType[K, V, A] =
      ParseErrorValue(error)

    def nonExistingValue[K, V, A](path: K, position: Option[Int]): ValueType[K, V, A] =
      NonExisting(path, position)

    def existingValue[K, V, A](a: A): ValueType[K, V, A] =
      Exists(a)
  }
}
