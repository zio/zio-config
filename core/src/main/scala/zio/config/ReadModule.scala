package zio.config

import zio.config.ReadError._
import VersionSpecificSupport._

private[config] trait ReadModule extends ConfigDescriptorModule {
  sealed trait ResultType[+A] {
    def value: A
    def map[B](f: A => B): ResultType[B] =
      this match {
        case ResultType.Raw(value)         => ResultType.raw(f(value))
        case ResultType.FallBack(value)    => ResultType.fallBack(f(value))
        case ResultType.OptionalRaw(value) => ResultType.optionalRaw(f(value))
      }
  }

  object ResultType {
    case class OptionalRaw[A](value: A) extends ResultType[A]
    case class Raw[A](value: A)         extends ResultType[A]
    case class FallBack[A](value: A)    extends ResultType[A]

    def raw[A](value: A): ResultType[A] =
      Raw(value)

    def fallBack[A](value: A): ResultType[A] =
      FallBack(value)

    def optionalRaw[A](value: A): ResultType[A] =
      OptionalRaw(value)
  }

  final def read[A](
    configuration: ConfigDescriptor[A]
  ): Either[ReadError[K], A] = {
    type Res[+B] = Either[ReadError[K], ResultType[B]]

    import ConfigDescriptorAdt._

    def formatError(paths: List[Step[K]], actualType: String, expectedType: String) =
      Left(
        ReadError.FormatError(
          paths.reverse,
          s"Provided value is of type $actualType, expecting the type $expectedType"
        )
      )

    def loopDefault[B](path: List[Step[K]], keys: List[K], cfg: Default[B], descriptions: List[String]): Res[B] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) if countMissingValueProductTerms(error) < cfg.config.productTerms => Left(error)
        case Left(_)                                                                       => Right(ResultType.FallBack(cfg.value))
        case Right(value)                                                                  => Right(value)
      }

    // Optional(Zip(...)
    def loopOptional[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Optional[B],
      descriptions: List[String]
    ): Res[Option[B]] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) =>
          println(s"the error is ${error}")
          error match {
            case ReadError.AndErrors(errors, anyOptionalValuePresent) if errors.forall(_.is({
                  case MissingValue(_, _) => true
                })) && error.cardinality >= cfg.config.requiredTerms && !anyOptionalValuePresent =>
              Right(ResultType.fallBack(None))
            case ReadError.OrErrors(errors) if errors.forall(_.is {
                  case MissingValue(_, _) => true
                }) =>
              Right(ResultType.fallBack(None))
            case ReadError.ListErrors(errors) if errors.forall(_.is {
                  case MissingValue(_, _) => true
                }) && error.cardinality >= cfg.config.requiredTerms =>
              Right(ResultType.fallBack(None))
            case ReadError.MapErrors(errors) if errors.forall(_.is {
                  case MissingValue(_, _) => true
                }) && error.cardinality >= cfg.config.requiredTerms =>
              Right(ResultType.fallBack(None))

            case MissingValue(_, _) =>
              Right(ResultType.fallBack((None)))
            case _ =>
              Left(Irrecoverable(List(error)))
          }
        case Right(value) =>
          Right(ResultType.optionalRaw(Some(value.value)))
      }

    def loopOrElse[B](path: List[Step[K]], keys: List[K], cfg: OrElse[B], descriptions: List[String]): Res[B] =
      loopAny(path, keys, cfg.left, descriptions) match {
        case a @ Right(_) => a
        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions) match {
            case a @ Right(_) => a
            case Left(rightError) =>
              Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopOrElseEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: OrElseEither[B, C],
      descriptions: List[String]
    ): Res[Either[B, C]] =
      loopAny(path, keys, cfg.left, descriptions) match {
        case Right(value) =>
          Right(value.map(Left(_)))

        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions) match {
            case Right(rightValue) =>
              Right(rightValue.map(Right(_)))

            case Left(rightError) =>
              Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopSource[B](path: List[Step[K]], keys: List[K], cfg: Source[B], descriptions: List[String]): Res[B] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Empty       => Left(ReadError.MissingValue(path.reverse, descriptions))
        case PropertyTree.Record(_)   => formatError(path, "Record", "Leaf")
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Leaf")
        case PropertyTree.Leaf(value) =>
          cfg.propertyType.read(value) match {
            case Left(parseError) =>
              Left(
                ReadError.FormatError(
                  path.reverse,
                  parseErrorMessage(
                    parseError.value.toString,
                    parseError.typeInfo
                  )
                )
              )
            case Right(parsed) => Right(ResultType.raw(parsed))
          }
      }

    def loopZip[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: Zip[B, C],
      descriptions: List[String]
    ): Res[(B, C)] =
      (loopAny(path, keys, cfg.left, descriptions), loopAny(path, keys, cfg.right, descriptions)) match {
        case (Right(leftV), Right(rightV)) =>
          (leftV, rightV) match {
            case (ResultType.FallBack(v1), v2) => Right(ResultType.fallBack((v1, v2.value)))
            case (v1, ResultType.FallBack(v2)) => Right(ResultType.fallBack((v1.value, v2)))
            case (v1, v2)                      => Right(ResultType.raw((v1.value, v2.value)))
          }

        case (Left(leftE), Left(rightE)) =>
          (leftE, rightE) match {
            case (AndErrors(_, fallBack1), AndErrors(_, fallBack2)) =>
              Left(AndErrors(leftE :: rightE :: Nil, fallBack1 || fallBack2))
            case (AndErrors(_, fallBack1), _) => Left(AndErrors(leftE :: rightE :: Nil, fallBack1))
            case (_, AndErrors(_, fallBack2)) => Left(AndErrors(leftE :: rightE :: Nil, fallBack2))
            case (_, _)                       => Left(AndErrors(leftE :: rightE :: Nil))
          }

        case (Left(leftE), Right(value)) =>
          value match {
            case ResultType.Raw(_)         => Left(AndErrors(List(leftE)))
            case ResultType.FallBack(_)    => Left(AndErrors(List(leftE)))
            case ResultType.OptionalRaw(_) => Left(AndErrors(List(leftE), anyOptionalValuePresent = true))
          }
        case (Right(value), Left(rightE)) =>
          value match {
            case ResultType.Raw(_)         => Left(AndErrors(List(rightE)))
            case ResultType.FallBack(_)    => Left(AndErrors(List(rightE)))
            case ResultType.OptionalRaw(_) => Left(AndErrors(List(rightE), anyOptionalValuePresent = true))
          }
      }

    def loopXmapEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: XmapEither[B, C],
      descriptions: List[String]
    ): Res[C] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) => Left(error)
        case Right(a) =>
          val result =
            a match {
              case ResultType.Raw(value)         => cfg.f(value).map(c => ResultType.raw(c))
              case ResultType.FallBack(value)    => cfg.f(value).map(c => ResultType.fallBack(c))
              case ResultType.OptionalRaw(value) => cfg.f(value).map(c => ResultType.optionalRaw(c))
            }

          result.swap.map(ReadError.ConversionError(path.reverse, _)).swap
      }

    def loopMap[B](path: List[Step[K]], keys: List[K], cfg: DynamicMap[B], descriptions: List[String]): Res[Map[K, B]] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Leaf(_)     => formatError(path, "Leaf", "Record")
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Record")
        case PropertyTree.Record(values) =>
          val result: List[(K, Res[B])] = values.toList.map {
            case ((k, tree)) =>
              val source: ConfigSource =
                getConfigSource(cfg.source.names, tree.getPath, cfg.source.leafForSequence)

              (k, loopAny(Step.Key(k) :: path, Nil, cfg.config.updateSource(_ => source), descriptions))
          }

          val map: Either[MapErrors[K], Map[K, B]] =
            seqMap2[K, ReadError[K], B](result.map({ case (a, b) => (a, b.map(_.value)) }).toMap).swap
              .map(errs => ReadError.MapErrors(errs))
              .swap

          map.map(mapp => ResultType.raw(mapp))

        case PropertyTree.Empty => Left(ReadError.MissingValue(path.reverse, descriptions))
      }

    def loopSequence[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Sequence[B],
      descriptions: List[String]
    ): Res[List[B]] = {
      def fromTrees(values: List[PropertyTree[K, V]]) = {
        val list = values.zipWithIndex.map {
          case (tree, idx) =>
            val source =
              getConfigSource(cfg.source.names, tree.getPath, cfg.source.leafForSequence)
            loopAny(
              Step.Index(idx) :: path,
              Nil,
              cfg.config.updateSource(_ => source),
              descriptions
            )
        }
        val result: Either[ListErrors[K], List[B]] =
          seqEither2[ReadError[K], B, ReadError[K]]((_, a) => a)(list.map(res => res.map(_.value))).swap
            .map(errs => ReadError.ListErrors(errs))
            .swap

        result.map(list => ResultType.raw(list))
      }

      cfg.source.getConfigValue(keys.reverse) match {
        case leaf @ PropertyTree.Leaf(_) =>
          cfg.source.leafForSequence match {
            case LeafForSequence.Invalid => formatError(path, "Leaf", "Sequence")
            case LeafForSequence.Valid   => fromTrees(List(leaf))
          }

        case PropertyTree.Record(_)        => formatError(path, "Record", "Sequence")
        case PropertyTree.Empty            => Left(ReadError.MissingValue(path.reverse, descriptions))
        case PropertyTree.Sequence(values) => fromTrees(values)
      }
    }

    def loopAny[B](
      path: List[Step[K]],
      keys: List[K],
      config: ConfigDescriptor[B],
      descriptions: List[String]
    ): Res[B] =
      config match {
        case c @ Default(_, _)        => loopDefault(path, keys, c, descriptions)
        case c @ Describe(_, message) => loopAny(path, keys, c.config, descriptions :+ message)
        case c @ DynamicMap(_, _)     => loopMap(path, keys, c, descriptions)
        case c @ Nested(_, _) =>
          loopAny(Step.Key(c.path) :: path, c.path :: keys, c.config, descriptions)

        case c @ Optional(_)         => loopOptional(path, keys, c, descriptions)
        case c @ OrElse(_, _)        => loopOrElse(path, keys, c, descriptions)
        case c @ OrElseEither(_, _)  => loopOrElseEither(path, keys, c, descriptions)
        case c @ Source(_, _)        => loopSource(path, keys, c, descriptions)
        case c @ Zip(_, _)           => loopZip(path, keys, c, descriptions)
        case c @ XmapEither(_, _, _) => loopXmapEither(path, keys, c, descriptions)
        case c @ Sequence(_, _)      => loopSequence(path, keys, c, descriptions)
      }

    loopAny(Nil, Nil, configuration, Nil).map(_.value)
  }

  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  /* def optionalCondition[A](requiredTerms: Int): PartialFunction[ReadError[K], Boolean] = {
    case error @ ReadError.AndErrors(errors, anyOptionalValuePresent) if errors.forall(_.is({
          case MissingValue(_, _) => true
        })) && error.cardinality == requiredTerms && !anyOptionalValuePresent => true
    case error @ ReadError.OrErrors(errors) => optionalCondition(error)
  }*/

  private def countMissingValueProductTerms[K](
    value: ReadError[K]
  ): Int = value match {
    case ReadError.FormatError(_, _)     => 0
    case ReadError.ConversionError(_, _) => 0
    case OrErrors(errors)                => errors.map(countMissingValueProductTerms).max
    case AndErrors(errors, _) =>
      errors.map(countMissingValueProductTerms).sum
    case MissingValue(_, _) => 1
    case _                  => 0
  }
}
