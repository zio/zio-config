package zio.config

import zio.{ IO, ZIO }
import zio.config.ConfigDescriptor.Sequence
import zio.config.ReadFunctions.ConfResult, ConfResult._
import zio.config.ReadFunctions._
import ReadFunctions.ConfResult
import zio.config.ReadError._

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): IO[ReadErrorsVector[K], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): ZIO[Any, Nothing, ::[ConfResult[Vector[K], ::[B]]]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          for {
            result <- source
                       .getConfigValue(paths :+ path)
                       .either
                       .map({
                         case Left(error) =>
                           singleton(errors(error): ConfResult[Vector[K], ::[B]])
                         case Right(opt) =>
                           opt match {
                             case Some(values) =>
                               mapCons(withIndex(values.value)) {
                                 case (Some(values), _) =>
                                   seqEitherCons(
                                     mapCons(values)(value => propertyType.read(value))
                                   ).fold(
                                     r =>
                                       errors(
                                         ReadError.ParseError(
                                           paths :+ path,
                                           ReadFunctions.parseErrorMessage(r.value.toString, r.typeInfo)
                                         )
                                       ),
                                     e => Exists[Vector[K], ::[B]](paths :+ path, e)
                                   )
                                 case (None, id) =>
                                   Errors(ReadError.MissingValue[Vector[K]](paths :+ path, Some(id)))
                               }

                             case None =>
                               singleton(
                                 errors(ReadError.MissingValue[Vector[K]](paths :+ path, None)): ConfResult[
                                   Vector[K],
                                   ::[B]
                                 ]
                               )
                           }
                       })
          } yield result

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s
          loop(config, paths).map(list => {
            singleton(seqConfResult(list))
              .asInstanceOf[::[ConfResult[Vector[K], ::[B]]]] // Required only for scala 2.12
          })

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths).flatMap { results =>
            foreach(results)(a => {
              ZIO
                .succeed(
                  a.flatMap(
                    values =>
                      seqEitherCons(mapCons(values)(f)) match {
                        case Left(value)  => Errors(ReadError.Unknown[Vector[K]](paths, new RuntimeException(value)))
                        case Right(value) => Exists(paths, value)
                      }
                  )
                )
            })
          }

        case ConfigDescriptor.Default(c, value) =>
          loop(c, paths).map(
            results => {
              mapCons(results)({
                case Exists(path, v) => Exists(path, v)
                case Errors(_)       => Exists(paths, singleton(value))
              })
            }
          )

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case cd: ConfigDescriptor.Optional[K, V1, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).map(
            results =>
              mapCons(results)({
                case Exists(path, v) => Exists(path, mapCons(v)(vv => Some(vv): Option[B]))
                case Errors(error) =>
                  if (hasNonFatalErrors(error) || hasParseErrors(error))
                    Errors(error)
                  else
                    Exists(paths, singleton(None: Option[B]))
              })
          )

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked => {
          val ConfigDescriptor.Zip(left, right) = r
          for {
            res1 <- loop(left, paths)
            res2 <- loop(right, paths)
            result2 = seqConfResult(res1) match {
              case Exists(_, v1) =>
                seqConfResult(res2) match {
                  case Exists(path, v2) =>
                    Exists[Vector[K], ::[(a, b)]](
                      path,
                      ::(v1.flatten.zip(v2.flatten).head, v1.flatten.zip(v2.flatten).tail)
                    ): ConfResult[Vector[K], ::[(a, b)]]
                  case Errors(error) =>
                    Errors[Vector[K]](error): ConfResult[Vector[K], ::[(a, b)]]
                }
              case Errors(error1) =>
                seqConfResult(res2) match {
                  case Exists(_, _) =>
                    Errors[Vector[K]](error1): ConfResult[Vector[K], ::[(a, b)]]
                  case Errors(error2) =>
                    Errors[Vector[K]](AndErrors(error1, error2)): ConfResult[Vector[K], ::[(a, b)]]
                }
            }
          } yield singleton(result2)
        }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd

          for {
            res1 <- loop(left, paths)
            res2 <- loop(right, paths)
            result = mapCons(withIndex(res1))({
              case (Errors(error1), id) =>
                res2.lift(id) match {
                  case Some(rightRes) =>
                    rightRes match {
                      case Exists(path, right) =>
                        Exists(path, mapCons(right)(Right(_))): ConfResult[Vector[K], ::[Either[a, b]]]
                      case Errors(error2) => Errors(OrErrors(error1, error2))
                    }
                  case None => Errors(OrErrors(error1, ReadError.MissingValue(paths, Some(id))))
                }

              case (Exists(key, value), _) =>
                Exists(key, mapCons(value)(Left(_))): ConfResult[Vector[K], ::[Either[a, b]]]
            })
          } yield result

        case ConfigDescriptor.OrElse(left, right) =>
          for {
            res1 <- loop(left, paths)
            res2 <- loop(right, paths)
            result = mapCons(withIndex(res1))({
              case (Errors(error1), id) =>
                res2.lift(id) match {
                  case Some(rightRes) =>
                    rightRes match {
                      case Exists(path, right) => Exists(path, right): ConfResult[Vector[K], ::[B]]
                      case Errors(error2)      => Errors(OrErrors(error1, error2))
                    }
                  case None => Errors(OrErrors(error1, ReadError.MissingValue(paths, Some(id))))
                }

              case (Exists(key, value), _) => Exists(key, value): ConfResult[Vector[K], ::[B]]
            })
          } yield result
      }

    loop(configuration, Vector.empty[K]).flatMap(values => {
      val errors = values.collect({ case Errors(error) => error })
      if (errors.nonEmpty)
        IO.fail(::(errors.head, errors.tail))
      else {
        val result = values.collect({ case Exists(_, v) => v })
        IO.succeed(result.head.head)
      }
    })
  }
}

object ReadFunctions {
  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  sealed trait ConfResult[+K, +B] { self =>

    def map[C](f: B => C): ConfResult[K, C] = self match {
      case Exists(path, v) => Exists(path, f(v))
      case Errors(errors)  => Errors(errors)
    }

    def flatMap[K1 >: K, C](f: B => ConfResult[K1, C]): ConfResult[K1, C] = self match {
      case Exists(_, v)  => f(v)
      case Errors(error) => Errors(error)
    }
  }

  object ConfResult {
    case class Exists[K, B](path: K, v: B)    extends ConfResult[K, B]
    case class Errors[K](error: ReadError[K]) extends ConfResult[K, Nothing]

    def errors[K](error: ReadError[K]): ConfResult[K, Nothing] =
      Errors(error)

    def seqConfResult[K, B](values: ::[ConfResult[K, B]]): ConfResult[K, ::[B]] = {
      val reversed = values.reverse
      reversed.tail.foldLeft(reversed.head.map(singleton))(
        (b, a) =>
          a match {
            case Exists(_, aa) =>
              b match {
                case Exists(path, bb) => Exists(path, ::(aa, bb))
                case Errors(error)    => Errors(error)
              }

            case Errors(error1) =>
              b match {
                case Exists(_, _)   => Errors(error1)
                case Errors(error2) => Errors(AndErrors(error1, error2))
              }
          }
      )
    }
  }

  final def hasNonFatalErrors[K, V1, B](value: ReadError[Vector[K]]): Boolean =
    value match {
      case ReadError.MissingValue(_, _) => false
      case ReadError.ParseError(_, _)   => false
      case ReadError.Unknown(_, _)      => true
      case ReadError.OrErrors(leftErrors, rightErrors) =>
        hasNonFatalErrors[K, V1, B](leftErrors) || hasNonFatalErrors[K, V1, B](rightErrors)
      case ReadError.AndErrors(leftErrors, rightErrors) =>
        hasNonFatalErrors[K, V1, B](leftErrors) || hasNonFatalErrors[K, V1, B](rightErrors)
    }

  final def hasParseErrors[K, V1, B](value: ReadError[Vector[K]]): Boolean =
    value match {
      case ReadError.MissingValue(_, _) => false
      case ReadError.ParseError(_, _)   => true
      case ReadError.Unknown(_, _)      => true
      case ReadError.OrErrors(leftErrors, rightErrors) =>
        hasParseErrors[K, V1, B](leftErrors) || hasParseErrors[K, V1, B](rightErrors)
      case ReadError.AndErrors(leftErrors, rightErrors) =>
        hasParseErrors[K, V1, B](leftErrors) || hasParseErrors[K, V1, B](rightErrors)
    }
}
