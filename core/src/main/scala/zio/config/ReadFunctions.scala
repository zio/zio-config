package zio.config

import zio.{IO, ZIO}
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
                               case (None, id) =>
                                 ValueType.nonExistingValue[Vector[K], V1, B](paths :+ path, Some(id))
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
                case ValueType.OrErrors(error1, error2) => ValueType.orErrors[Vector[K], V1, B](error1.asInstanceOf[ValueType[Vector[K], V1, B]], error2.asInstanceOf[ValueType[Vector[K], V1, B]])
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
              val res = mapCons(value)(t => Some(t): Option[B])
              ZIO.succeed(res)
            case Left(r) =>
              println(s"the  is ${r}")
              if (r.exists(_.isNonFatalError) || r.exists(_.isParseError))
                ZIO.fail(r)
              else
                ZIO.succeed({
                  val result = r.collect({
                    case ValueType.Exists(a)         => Some(a)
                    case ValueType.NonExisting(_, _) => None
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
                    val res = aa.zipWithIndex.map({
                      case (ValueType.Exists(a), id) =>
                        b.lift(id) match {
                          case Some(v) => ValueType.existingValue[Vector[K], V1, (a, b)]((a, v))
                          case None =>
                            ValueType.nonExistingValue[Vector[K], V1, (a, b)](paths, Some(id))
                        }

                      case (ValueType.NonExisting(path, position), _) => ValueType.nonExistingValue[Vector[K], V1, (a, b)](path, position)
                      case (ValueType.ParseErrorValue(error), _) =>    ValueType.parseErrorValue[Vector[K], V1, (a, b)](error)
                      case (ValueType.NonFatalErrorValue(error), _) =>
                        ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error)
                      case (ValueType.OrErrors(error1, error2), _) =>
                        ValueType.orErrors[Vector[K], V1, (a, b)](error1.asInstanceOf[ValueType[Vector[K], V1, (a, b)]], error2.asInstanceOf[ValueType[Vector[K], V1, (a, b)]])

                    })

                    ZIO.fail(::(res.head, res.tail))

                  case (Right(aa), Left(bb)) =>
                    println(s"is it this? ${bb}")
                    val res = bb.zipWithIndex.map({
                      case (ValueType.Exists(a), id) =>
                        aa.lift(id) match {
                          case Some(v) => ValueType.existingValue[Vector[K], V1, (a, b)]((v, a))
                          case None =>
                            ValueType.nonExistingValue[Vector[K], V1, (a, b)](paths, Some(id))
                        }

                      case (ValueType.NonExisting(path, position), _) => ValueType.nonExistingValue[Vector[K], V1, (a, b)](path, position)
                      case (ValueType.ParseErrorValue(error), _) =>
                        println("hey?")
                        ValueType.parseErrorValue[Vector[K], V1, (a, b)](error)
                      case (ValueType.NonFatalErrorValue(error), _) =>
                        ValueType.nonFatalErrorValue[Vector[K], V1, (a, b)](error)
                      case (ValueType.OrErrors(error1, error2), _) => throw new Exception

                    })

                    println(::(res.head, res.tail))

                    ZIO.fail(::(res.head, res.tail))

                  case (Left(err1), Left(err2)) =>
                    val res1 = err1.filterNot(_.isSuccess) ++ err2.filterNot(_.isSuccess)
                    val res = res1.asInstanceOf[::[ValueType[Vector[K], V1, B]]]
                    ZIO.fail(::(res.head, res.tail))
                }
          } yield r
        }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd
          loop(left, paths).either
            .flatMap(
              {
                // if the lef side wins,that's it
                case Right(a) =>
                  val result = mapCons(a)(r => Left(r): Either[a, b])
                  ZIO.succeed(result)

                    // If the left side fails, and if left side failure reasons is fatal, then fail the job, needn't accumulate errors.
                case Left(lerr) =>
                  println("or else either")
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
                    // If there was no catastrophic error involved, then continue accumulating.
                  } else {
                    println(s"and it reaches here. ${lerr}")
                    // try out right side, and if a value exists,
                    loop(right, paths).either.flatMap({
                      case Right(b) =>
                        val result = lerr.zipWithIndex.map({
                          case (ValueType.Exists(a), _) =>
                            // If left already exists, use it !
                            ValueType.existingValue[Vector[K], V1, Either[a, b]](Left(a))

                            // If left is non existing, then try and get the value from right as well, if not both are non existing values
                          case a @ (ValueType.NonExisting(path, position), id) =>
                            b.lift(id) match {
                              case Some(value) =>
                                ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(value))
                              case None        => ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]], ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](path, position))
                            }

                          // If left is non parse errors, then try and get the value from right as well, if not parse errors.
                          case left @ (ValueType.ParseErrorValue(_), id) =>
                            println("is it here?")
                            b.lift(id) match {
                              case Some(value) => ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(value))
                              case None        => ValueType.orErrors[Vector[K], V1, Either[a, b]](left._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]], ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](paths, Some(id)))
                            }

                            // If left is non fatal errors, then We don't care anymore
                          case left @ (ValueType.NonFatalErrorValue(error), id) =>
                            ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)

                            // will seee!
                          case (ValueType.OrErrors(error1, error2), _) => throw new Exception

                        })

                        println(s"this bloddy things ${result}")

                        ZIO.succeed({
                          val r = result.collect({ case ValueType.Exists(a) => a })
                          ::(r.head, r.tail)
                        })

                        // Now both left and right are tried out and we ed up
                      case Left(rerr) =>
                        println(s"it can be left or right errors. ${lerr}, ${rerr}")
                        // Fail the  it if right side consist of non fatal errors, then remvoe all success values, concatenate the errors and just fail no dramas
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
                          println(s"fix it ! ${lerr}, ${rerr}")
                          // If there are no fatal errors yet, for right side, however still failed
                          val result = lerr.zipWithIndex.map({
                            case (ValueType.Exists(a), _) =>
                              ValueType.existingValue[Vector[K], V1, Either[a, b]](Left(a): Either[a, b])
                            case a @ (ValueType.NonExisting(k, v), id) =>
                              rerr.lift(id) match {
                                case Some(value) =>
                                  value match {
                                    case ValueType.Exists(v) =>
                                      ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(v): Either[a, b])
                                    case ValueType.NonExisting(k, v) =>
                                      ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]],   ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](k, v))
                                    case ValueType.ParseErrorValue(error) =>
                                      ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]],  ValueType.parseErrorValue[Vector[K], V1, Either[a, b]](error))
                                    case ValueType.NonFatalErrorValue(error) =>
                                      ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error)
                                    case (ValueType.OrErrors(error1, error2)) => throw new Exception

                                  }

                                case None => ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](paths, None)
                              }

                            case (ValueType.OrErrors(error1, error2), _) => throw new Exception
                            case a @ (ValueType.ParseErrorValue(ttt), id) =>
                              println("ea, parse errors on the left side and what do we do with right side " + lerr + "    " + rerr)

                              rerr.lift(id) match {
                                case Some(value) =>
                                  value match {
                                    case ValueType.Exists(v) =>
                                      ValueType.existingValue[Vector[K], V1, Either[a, b]](Right(v): Either[a, b])
                                    case ValueType.NonExisting(k, v) =>
                                      ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]], ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](k, v))
                                    case ValueType.ParseErrorValue(error) =>
                                      ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]], ValueType.parseErrorValue[Vector[K], V1, Either[a, b]](error))
                                    case ValueType.NonFatalErrorValue(error) =>
                                      ValueType.orErrors[Vector[K], V1, Either[a, b]](a._1.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]],  ValueType.nonFatalErrorValue[Vector[K], V1, Either[a, b]](error))
                                    case (ValueType.OrErrors(error1, error2)) => throw new Exception

                                  }
                                case None => ValueType.orErrors[Vector[K], V1, Either[a, b]](a.asInstanceOf[ValueType[Vector[K], V1, Either[a, b]]], ValueType.nonExistingValue[Vector[K], V1, Either[a, b]](paths, None))
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
                          case (ValueType.OrErrors(error1, error2), _) => throw new Exception
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
                                  case (ValueType.OrErrors(error1, error2)) => throw new Exception

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

    def partial: PartialFunction[ValueType[Vector[K], V, A], ReadErrorsVector[K, V]] = {
      case ValueType.NonExisting(paths, index) =>
        ::(ReadError.missingValue[Vector[K], V](paths, index), Nil)
      case ValueType.ParseErrorValue(error)    => ::(error: ReadError[Vector[K], V], Nil)
      case ValueType.OrErrors(error1, error2) =>  concat(partial(error1), partial(error2))
      case ValueType.NonFatalErrorValue(error) => ::(error, Nil)
    }

    loop(configuration, Vector.empty[K]).either
      .map({
        case Left(value) =>
          val res = value
            .filterNot(_.isSuccess)
            .collect(partial)
          Left(::(res.flatten.head, res.flatten.tail))
        case Right(value) => Right(value)
      })
      .absolve
      .map(_.head)
  }
}

object ReadFunctions {
  sealed trait ValueType[K, V, +A] { self =>
    def isSuccess: Boolean = self match {
      case ValueType.Exists(_) => true
      case ValueType.OrErrors(error1, error2) => error1.isSuccess || error2.isSuccess
      case _                   => false
    }

    def isParseError: Boolean = self match {
      case ValueType.ParseErrorValue(_) => true
      case ValueType.OrErrors(error1, error2) => error1.isParseError || error2.isParseError
      case _                            => false
    }

    def isNonFatalError: Boolean = self match {
      case ValueType.NonFatalErrorValue(_) => true
      case ValueType.OrErrors(error1, error2) => error1.isParseError || error2.isParseError
      case _                               => false
    }
  }

  object ValueType {

    case class Exists[K, V, A](a: A) extends ValueType[K, V, A]

    case class NonExisting[K, V, A](path: K, position: Option[Int]) extends ValueType[K, V, A]

    case class ParseErrorValue[K, V, A](error: ParseError[K, V]) extends ValueType[K, V, A]

    case class NonFatalErrorValue[K, V, A](error: ReadError.Unknown[K]) extends ValueType[K, V, A]

    case class OrErrors[K, V, A](leftErrors: ValueType[K, V, A], rightErrors: ValueType[K, V, A]) extends ValueType[K, V, A]

    def nonFatalErrorValue[K, V, A](error: ReadError.Unknown[K]): ValueType[K, V, A] =
      NonFatalErrorValue(error)

    def parseErrorValue[K, V, A](error: ParseError[K, V]): ValueType[K, V, A] =
      ParseErrorValue(error)

    def nonExistingValue[K, V, A](path: K, position: Option[Int]): ValueType[K, V, A] =
      NonExisting(path, position)

    def existingValue[K, V, A](a: A): ValueType[K, V, A] =
      Exists(a)

    def orErrors[K, V, A](a: ValueType[K, V, A], b: ValueType[K, V, A]): ValueType[K, V, A] =
      OrErrors(a, b)
  }
}
