package zio.config

import zio.config.ReadError._
import VersionSpecificSupport._

private[config] trait ReadModule extends ConfigDescriptorModule {
  sealed trait ResultType[+A] {
    def value: A
    def map[B](f: A => B): ResultType[B] =
      this match {
        case ResultType.Raw(value)             => ResultType.raw(f(value))
        case ResultType.DefaultValue(value)    => ResultType.defaultValue(f(value))
        case ResultType.NonDefaultValue(value) => ResultType.nonDefaultValue(f(value))
      }
  }

  object ResultType {
    case class NonDefaultValue[A](value: A) extends ResultType[A]
    case class Raw[A](value: A)             extends ResultType[A]
    case class DefaultValue[A](value: A)    extends ResultType[A]

    def raw[A](value: A): ResultType[A] =
      Raw(value)

    def defaultValue[A](value: A): ResultType[A] =
      DefaultValue(value)

    def nonDefaultValue[A](value: A): ResultType[A] =
      NonDefaultValue(value)
  }

  final def read[A](
    configuration: ConfigDescriptor[A]
  ): Either[ReadError[K], A] = {
    type Res[+B] = Either[ReadError[K], ResultType[B]]

    import ConfigDescriptorAdt._

    def formatError(paths: List[Step[K]], actualType: String, expectedType: String, descriptions: List[String]) =
      Left(
        ReadError.FormatError(
          paths.reverse,
          s"Provided value is of type $actualType, expecting the type $expectedType",
          descriptions
        )
      )

    // Optional(Zip(...)
    def loopDefault[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Default[B],
      descriptions: List[String]
    ): Res[B] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) =>
          error match {
            case ReadError.ZipErrors(errors, anyDefaultValuePresent)
                if errors.forall(_.allMissingValues) && error.cardinality == cfg.config.requiredTerms && !anyDefaultValuePresent =>
              Right(ResultType.defaultValue(cfg.default))

            case ReadError.OrErrors(errors)
                if errors.forall(_.allMissingValues) &&
                  error.cardinality == cfg.config.requiredTerms &&
                  !appliedNonDefaultValue(error) =>
              Right(ResultType.defaultValue(cfg.default))

            case MissingValue(_, _) =>
              Right(ResultType.defaultValue(cfg.default))

            case _ =>
              Left(Irrecoverable(List(error)))
          }
        case Right(value) =>
          Right(ResultType.nonDefaultValue(value.value))
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
        case PropertyTree.Record(_)   => formatError(path, "Record", "Leaf", descriptions)
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Leaf", descriptions)
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
          // Not really required
          (leftV, rightV) match {
            case (ResultType.DefaultValue(v1), v2) => Right(ResultType.defaultValue((v1, v2.value)))
            case (v1, ResultType.DefaultValue(v2)) => Right(ResultType.defaultValue((v1.value, v2)))
            case (v1, v2)                          => Right(ResultType.raw((v1.value, v2.value)))
          }

        case (Left(leftE), Left(rightE)) =>
          (leftE, rightE) match {
            case (ZipErrors(_, fallBack1), ZipErrors(_, fallBack2)) =>
              Left(ZipErrors(leftE :: rightE :: Nil, fallBack1 || fallBack2))
            case (ZipErrors(_, fallBack1), _) => Left(ZipErrors(leftE :: rightE :: Nil, fallBack1))
            case (_, ZipErrors(_, fallBack2)) => Left(ZipErrors(leftE :: rightE :: Nil, fallBack2))
            case (_, _)                       => Left(ZipErrors(leftE :: rightE :: Nil))
          }

        case (Left(leftE), Right(value)) =>
          value match {
            case ResultType.Raw(_)             => Left(ZipErrors(List(leftE)))
            case ResultType.DefaultValue(_)    => Left(ZipErrors(List(leftE)))
            case ResultType.NonDefaultValue(_) => Left(ZipErrors(List(leftE), anyNonDefaultValue = true))
          }
        case (Right(value), Left(rightE)) =>
          value match {
            case ResultType.Raw(_)             => Left(ZipErrors(List(rightE)))
            case ResultType.DefaultValue(_)    => Left(ZipErrors(List(rightE)))
            case ResultType.NonDefaultValue(_) => Left(ZipErrors(List(rightE), anyNonDefaultValue = true))
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
              case ResultType.Raw(value)             => cfg.f(value).map(c => ResultType.raw(c))
              case ResultType.DefaultValue(value)    => cfg.f(value).map(c => ResultType.defaultValue(c))
              case ResultType.NonDefaultValue(value) => cfg.f(value).map(c => ResultType.nonDefaultValue(c))
            }

          result.swap.map(ReadError.ConversionError(path.reverse, _)).swap
      }

    def loopMap[B](path: List[Step[K]], keys: List[K], cfg: DynamicMap[B], descriptions: List[String]): Res[Map[K, B]] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Leaf(_)     => formatError(path, "Leaf", "Record", descriptions)
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Record", descriptions)
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
            case LeafForSequence.Invalid => formatError(path, "Leaf", "Sequence", descriptions)
            case LeafForSequence.Valid   => fromTrees(List(leaf))
          }

        case PropertyTree.Record(_)        => formatError(path, "Record", "Sequence", descriptions)
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

  def appliedNonDefaultValue(error: ReadError[K]): Boolean =
    error match {
      case ZipErrors(_, anyNonDefaultValue) => anyNonDefaultValue
      case OrErrors(list)                   => list.exists(e => appliedNonDefaultValue(e))
      case ListErrors(list)                 => list.exists(appliedNonDefaultValue)
      case MapErrors(list)                  => list.exists(appliedNonDefaultValue)
      case MissingValue(_, _)               => false
      case FormatError(_, _, _)             => false
      case ConversionError(_, _)            => false
      case Irrecoverable(_)                 => false
    }

}
