package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ReadError.{ AndErrors, OrErrors }
import zio.config.ReadFunctions._

private[config] trait ReadFunctions {
  final def read[K, V, A](
    configuration: ConfigDescriptor[K, V, A]
  ): Either[ReadError[K], A] = {
    def loop[V1, B](
      configuration: ConfigDescriptor[K, V1, B],
      keys: Vector[K],
      paths: Vector[Either[Int, K]]
    ): PropertyTree[K, Either[ReadError[K], B]] =
      configuration match {
        case ConfigDescriptor.Source(key, source: ConfigSource[K, V1], propertyType: PropertyType[V1, B]) =>
          source
            .getConfigValue(keys :+ key) match {
            case Some(tree) =>
              tree
                .mapEmptyToError(ReadError.MissingValue((paths :+ Right(key))))
                .map({
                  case Left(value) => Left(value)
                  case Right(value) =>
                    propertyType.read(value) match {
                      case Left(value) =>
                        Left(
                          ReadError.FormatError(
                            (paths :+ Right(key)),
                            ReadFunctions.parseErrorMessage(value.value.toString, value.typeInfo)
                          )
                        )
                      case Right(value) => Right(value)
                    }
                })
            case None => throw new Exception()
          }

        case s: Sequence[K, V1, B] @unchecked =>
          val Sequence(config) = s

          loop(config, keys, paths).map(_.map(_ :: Nil)).reduceInner[Either[ReadError[K], List[B]]] {
            case (Right(l), Right(r)) => Right(l ++ r)
            case (Left(l), Right(_))  => Left(l)
            case (Right(_), Left(r))  => Left(r)
            case (Left(l), Left(r))   => Left(ReadError.AndErrors(l :: r :: Nil))
          }

        //required

        case ConfigDescriptor.Nested(path, c) =>
          loop(c, keys :+ path, paths :+ Right(path))

        case cd: ConfigDescriptor.XmapEither[K, V1, a, B] =>
          val ConfigDescriptor.XmapEither(c, f, _) = cd
          loop(c, keys, paths).map {
            case Left(value) => Left(value)
            case Right(value) =>
              f(value) match {
                case Left(value) => Left(ReadError.ConversionError(paths, value))
                case Right(value) =>
                  Right(value)
              }
          }

        case cd: ConfigDescriptor.Default[K, V1, B] =>
          val ConfigDescriptor.Default(config, value) = cd
          loop(config, keys, paths).map {
            case Left(error) if (hasParseErrors(error) || hasNonFatalErrors(error)) => Left(error)
            case Left(_)                                                            => Right(value)
            case Right(value)                                                       => Right(value)
          }

        case ConfigDescriptor.Describe(c, _) =>
          loop(c, keys, paths)

        case cd: ConfigDescriptor.Optional[K, V1, B] @unchecked =>
          val ConfigDescriptor.Optional(c) = cd
          loop(c, keys, paths).map {
            case Left(error) if (hasParseErrors(error) || hasNonFatalErrors(error)) => Left(error)
            case Left(_) =>
              Right(None)
            case Right(value) =>
              Right(Some(value))
          }

        case r: ConfigDescriptor.Zip[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.Zip(left, right) = r

          val lefts  = loop(left, keys, paths)
          val rights = loop(right, keys, paths)
          val zippedRes = (lefts, rights) match {
            case (l, r) =>
              val res = l.zipWith(r) { (l, r) =>
                (l, r) match {
                  case (Left(l), Left(r))   => Left(AndErrors(List(l, r)))
                  case (Left(l), Right(r))  => Left(l)
                  case (Right(l), Left(r))  => Left(r)
                  case (Right(l), Right(r)) => Right((l, r))
                }
              }

              res
          }
          zippedRes

        case cd: ConfigDescriptor.OrElseEither[K, V1, a, b] @unchecked =>
          val ConfigDescriptor.OrElseEither(left, right) = cd

          val left1 =
            loop(left, keys, paths)

          val errorsInLeft =
            errors(left1)

          errorsInLeft match {
            case Some(error) if hasNonFatalErrors(error) =>
              left1.map({
                case Left(value)  => Left(value)
                case Right(value) => Right(Left(value): Either[a, b])
              })
            case Some(_) =>
              (loop(left, keys, paths), loop(right, keys, paths)) match {
                case (l, r) =>
                  PropertyTree.orElseEither(l, r)((a, b) => OrErrors(List(a, b)))
              }
            case None =>
              left1.map({
                case Left(value)  => Left(value)
                case Right(value) => Right(Left(value): Either[a, b])
              })
          }

        case ConfigDescriptor.OrElse(left, right) =>
          val left1 =
            loop(left, keys, paths)

          val errorsInLeft =
            errors(left1)

          errorsInLeft match {
            case Some(error) if hasNonFatalErrors(error) =>
              left1
            case Some(_) =>
              (loop(left, keys, paths), loop(right, keys, paths)) match {
                case (l, r) =>
                  PropertyTree.orElse(l, r)((a, b) => OrErrors(List(a, b)))
              }
            case None =>
              left1
          }
      }

    val tree =
      loop(configuration, Vector.empty, Vector.empty)

    ReadFunctions.errors(tree) match {
      case Some(value) =>
        Left(value)
      case None =>
        println(s"the final result is ...................................... ${tree}")
        PropertyTree.getValue(tree)

    }

  }
}

object ReadFunctions {
  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  final def hasErrors[K](value: ReadError[K])(f: PartialFunction[ReadError[K], Boolean]): Boolean =
    f.orElse[ReadError[K], Boolean]({
        case OrErrors(errors)  => errors.exists(hasErrors(_)(f))
        case AndErrors(errors) => errors.exists(hasErrors(_)(f))
      })
      .lift(value)
      .getOrElse(false)

  final def hasParseErrors[K](error: ReadError[K]): Boolean =
    hasErrors(error)({ case ReadError.FormatError(_, _) => true })

  final def hasNonFatalErrors[K](error: ReadError[K]): Boolean =
    hasErrors(error)({ case ReadError.ConversionError(_, _) => true })

  final def errors[K, V1, B](tree: PropertyTree[K, Either[ReadError[K], B]]): Option[ReadError[K]] = {
    def loop(tree: PropertyTree[K, Either[ReadError[K], B]], acc: Option[ReadError[K]]): Option[ReadError[K]] =
      tree match {
        case PropertyTree.Leaf(value) =>
          value match {
            case Left(value) =>
              Some(acc.fold(value)({
                case AndErrors(list) => AndErrors(value :: list)
                case errors          => AndErrors(List(errors, value))
              }))
            case Right(_) => acc
          }
        case PropertyTree.Record(value) =>
          value.toList.map(_._2).map(tree => loop(tree, acc)).foldLeft(None: Option[ReadError[K]]) { (acc, a) =>
            (acc, a) match {
              case (Some(l), Some(r)) => Some(AndErrors(List(l, r)))
              case (_, Some(value))   => Some(value)
              case (Some(value), _)   => Some(value)
              case (None, None)       => None
            }
          }
        case PropertyTree.Sequence(value) =>
          value.map(tree => loop(tree, acc)).foldLeft(None: Option[ReadError[K]]) { (acc, a) =>
            (acc, a) match {
              case (Some(l), Some(r)) => Some(AndErrors(List(l, r)))
              case (_, Some(value))   => Some(value)
              case (Some(value), _)   => Some(value)
              case (None, None)       => None
            }
          }

        case PropertyTree.Empty => None
      }

    loop(tree, None)
  }
}
