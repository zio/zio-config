package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ReadError.{ AndErrors, OrErrors }
import zio.config.ReadFunctions._

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): Either[ReadError, A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      paths: Vector[K]
    ): PropertyTree[K, Either[ReadError, B]] =
      configuration match {
        case ConfigDescriptor.Source(path, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          source.getConfigValue(paths :+ path) match {
            case Some(tree) =>
              tree.map(
                v =>
                  propertyType.read(v) match {
                    case Left(value) =>
                      Left(
                        ReadError.ParseError(
                          (paths :+ path).toString,
                          ReadFunctions.parseErrorMessage(value.value.toString, value.typeInfo)
                        )
                      )
                    case Right(value) => Right(value)
                  }
              )
            case None =>
              PropertyTree.Sequence(
                List(PropertyTree.Leaf(Left(ReadError.MissingValue((paths :+ path).toString(), None))))
              )

          }

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s
          PropertyTree.sequence(loop(config, paths)) match {
            case Some(value) => value.map(seqEither(_))
            case None =>
              PropertyTree.Sequence(List(PropertyTree.Leaf(Left(ReadError.MissingValue(paths.toString(), None)))))
          }

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, paths :+ path)

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, paths).map {
            case Left(value) => Left(value)
            case Right(value) =>
              f(value) match {
                case Left(value)  => Left(ReadError.Unknown(paths.toString, new RuntimeException(value)))
                case Right(value) => Right(value)
              }
          }

        case cd: ConfigDescriptor.Default[K, V1, B] =>
          val ConfigDescriptor.Default(config, value) = cd
          loop(config, paths).map {
            case Left(error) if (hasParseErrors(error) || hasNonFatalErrors(error)) => Left(error)
            case Left(_)                                                            => Right(value)
            case Right(value)                                                       => Right(value)
          }

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, paths)

        case cd: ConfigDescriptor.Optional[K, V1, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, paths).map {
            case Left(error) if (hasParseErrors(error) || hasNonFatalErrors(error)) => Left(error)
            case Left(_)                                                            => Right(None)
            case Right(value)                                                       => Right(Some(value))
          }

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.Zip(left, right) = r

          (loop(left, paths), loop(right, paths)) match {
            case (l, r) =>
              println(s"in zip folder. ${l}, ${r}")
              l.map2(r) { (l, r) =>
                (l, r) match {
                  case (Left(l), Left(r))   => Left(AndErrors(l, r))
                  case (Left(l), Right(r))  => Left(l)
                  case (Right(l), Left(r))  => Left(r)
                  case (Right(l), Right(r)) => Right((l, r))
                }
              } match {
                case Some(value) => value
                case None =>
                  PropertyTree.Sequence(List(PropertyTree.Leaf(Left(ReadError.MissingValue(paths.toString(), None)))))
              }
          }

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd

          (loop(left, paths), loop(right, paths)) match {
            case (l, r) =>
              PropertyTree.orElseEither(l, r)((a, b) => OrErrors(a, b)) match {
                case Some(value) => value
                case None =>
                  PropertyTree.Sequence(List(PropertyTree.Leaf(Left(ReadError.MissingValue(paths.toString(), None)))))
              }
          }

        case ConfigDescriptor.OrElse(left, right) =>
          (loop(left, paths), loop(right, paths)) match {
            case (l, r) =>
              PropertyTree.orElse(l, r)((a, b) => OrErrors(a, b)) match {
                case Some(value) => value
                case None =>
                  PropertyTree.Sequence(List(PropertyTree.Leaf(Left(ReadError.MissingValue(paths.toString(), None)))))
              }
          }
      }

    val tree =
      loop(configuration, Vector.empty[K])

    ReadFunctions.errors(tree) match {
      case Some(value) =>
        Left(value)
      case None =>
        println("the result is tree " + tree)
        tree match {
          case PropertyTree.Leaf(value) => value
          case PropertyTree.Record(_)   => throw new Exception("ah!")
          case PropertyTree.Sequence(value) =>
            value.head match {
              case PropertyTree.Leaf(value) =>
                println("is it here?")
                value
              case _ => throw new Exception("ah!")
            }
        }

    }

  }
}

object ReadFunctions {
  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  final def hasNonFatalErrors(value: ReadError): Boolean =
    value match {
      case ReadError.MissingValue(_, _) => false
      case ReadError.ParseError(_, _)   => false
      case ReadError.Unknown(_, _)      => true
      case ReadError.OrErrors(leftErrors, rightErrors) =>
        hasNonFatalErrors(leftErrors) || hasNonFatalErrors(rightErrors)
      case ReadError.AndErrors(leftErrors, rightErrors) =>
        hasNonFatalErrors(leftErrors) || hasNonFatalErrors(rightErrors)
    }

  final def hasParseErrors[K, V1, B](value: ReadError): Boolean =
    value match {
      case ReadError.MissingValue(_, _) => false
      case ReadError.ParseError(_, _)   => true
      case ReadError.Unknown(_, _)      => true
      case ReadError.OrErrors(leftErrors, rightErrors) =>
        hasParseErrors(leftErrors) || hasParseErrors(rightErrors)
      case ReadError.AndErrors(leftErrors, rightErrors) =>
        hasParseErrors(leftErrors) || hasParseErrors(rightErrors)
    }

  final def errors[K, V1, B](tree: PropertyTree[K, Either[ReadError, B]]): Option[ReadError] = {
    def loop(tree: PropertyTree[K, Either[ReadError, B]], acc: Option[ReadError]): Option[ReadError] =
      tree match {
        case PropertyTree.Leaf(value) =>
          value match {
            case Left(value) => Some(acc.fold(value)(res => AndErrors(res, value)))
            case Right(_)    => acc
          }
        case PropertyTree.Record(value) =>
          value.toList.map(_._2).map(tree => loop(tree, acc)).foldLeft(None: Option[ReadError]) { (acc, a) =>
            (acc, a) match {
              case (Some(l), Some(r)) => Some(AndErrors(l, r))
              case (_, Some(value))   => Some(value)
              case (Some(value), _)   => Some(value)
              case (None, None)       => None
            }
          }
        case PropertyTree.Sequence(value) =>
          value.map(tree => loop(tree, acc)).foldLeft(None: Option[ReadError]) { (acc, a) =>
            (acc, a) match {
              case (Some(l), Some(r)) => Some(AndErrors(l, r))
              case (_, Some(value))   => Some(value)
              case (Some(value), _)   => Some(value)
              case (None, None)       => None
            }
          }
      }

    loop(tree, None)
  }
}
