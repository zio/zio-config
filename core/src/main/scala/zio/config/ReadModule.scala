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

    def mapError[E, B](f: A => Either[E, B]): Either[E, ResultType[B]] = {
      def fromEither(either: Either[E, B], f: B => ResultType[B]): Either[E, ResultType[B]] =
        either match {
          case Left(value)  => Left(value)
          case Right(value) => Right(f(value))
        }

      map(f) match {
        case ResultType.NonDefaultValue(value) => fromEither(value, ResultType.nonDefaultValue)
        case ResultType.Raw(value)             => fromEither(value, ResultType.raw)
        case ResultType.DefaultValue(value)    => fromEither(value, ResultType.defaultValue)
      }
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
    def loopOptional[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Optional[B],
      descriptions: List[String]
    ): Res[Option[B]] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) =>
          handleOption(error, cfg.config, None)

        case Right(value) =>
          Right(ResultType.nonDefaultValue(Some(value.value)))
      }

    def loopDefault[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Default[B],
      descriptions: List[String]
    ): Res[B] =
      loopAny(path, keys, cfg.config, descriptions) match {
        case Left(error) =>
          handleOption(error, cfg.config, cfg.default)

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
    ): Res[(B, C)] = {

      def carryForwardNonDefaultValueApplied[V](error: ReadError[K], value: ResultType[V]) = {
        def result(defaultValueApplied: Boolean) =
          value match {
            case ResultType.Raw(_)             => Left(ZipErrors(List(error), defaultValueApplied))
            case ResultType.DefaultValue(_)    => Left(ZipErrors(List(error), defaultValueApplied))
            case ResultType.NonDefaultValue(_) => Left(ZipErrors(List(error), anyNonDefaultValue = true))
          }

        error match {
          case ZipErrors(_, anyNonDefaultValue) => result(anyNonDefaultValue)
          case _                                => result(false)
        }
      }

      (loopAny(path, keys, cfg.left, descriptions), loopAny(path, keys, cfg.right, descriptions)) match {
        case (Right(leftV), Right(rightV)) =>
          (leftV, rightV) match {
            case (ResultType.NonDefaultValue(v1), v2) => Right(ResultType.nonDefaultValue((v1, v2.value)))
            case (v1, ResultType.NonDefaultValue(v2)) => Right(ResultType.nonDefaultValue((v1.value, v2)))
            case (v1, v2)                             => Right(ResultType.raw((v1.value, v2.value)))
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
          carryForwardNonDefaultValueApplied(leftE, value)

        case (Right(value), Left(rightE)) =>
          carryForwardNonDefaultValueApplied(rightE, value)
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
          a.mapError(cfg.f).swap.map(ReadError.ConversionError(path.reverse, _)).swap
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

          seqMap2[K, ReadError[K], B](result.map({ case (a, b) => (a, b.map(_.value)) }).toMap).swap
            .map(errs => ReadError.MapErrors(errs))
            .swap
            .map(mapp => ResultType.raw(mapp))

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

        seqEither2[ReadError[K], B, ReadError[K]]((_, a) => a)(list.map(res => res.map(_.value))).swap
          .map(errs => ReadError.ListErrors(errs))
          .swap
          .map(list => ResultType.raw(list))
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
        case c @ Nested(_, _)         => loopAny(Step.Key(c.path) :: path, c.path :: keys, c.config, descriptions)
        case c @ Optional(_)          => loopOptional(path, keys, c, descriptions)
        case c @ OrElse(_, _)         => loopOrElse(path, keys, c, descriptions)
        case c @ OrElseEither(_, _)   => loopOrElseEither(path, keys, c, descriptions)
        case c @ Source(_, _)         => loopSource(path, keys, c, descriptions)
        case c @ Zip(_, _)            => loopZip(path, keys, c, descriptions)
        case c @ XmapEither(_, _, _)  => loopXmapEither(path, keys, c, descriptions)
        case c @ Sequence(_, _)       => loopSequence(path, keys, c, descriptions)
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

  def handleOption[A, B](
    error: ReadError[K],
    config: ConfigDescriptor[A],
    default: B
  ): Either[ReadError[K], ResultType[B]] = {
    def hasOnlyMissingValuesIfNotIrrecoverable(error: ReadError[K]): Boolean =
      error.fold(false) {
        case ReadError.MissingValue(_, _) => true
        case ReadError.Irrecoverable(_)   => false
      }(_ && _, true)

    def checkIfAllRequiredValuesAreMissing(errors: List[ReadError[K]]): Boolean =
      errors.forall(hasOnlyMissingValuesIfNotIrrecoverable) && error.sizeOfZipAndOrErrors == config.requiredTerms

    error match {
      case MissingValue(_, _) => Right(ResultType.defaultValue(default))
      case ReadError.ZipErrors(errors, anyDefaultValuePresent)
          if checkIfAllRequiredValuesAreMissing(errors) && !anyDefaultValuePresent =>
        Right(ResultType.defaultValue(default))

      case ReadError.OrErrors(errors) if checkIfAllRequiredValuesAreMissing(errors) && !appliedNonDefaultValue(error) =>
        Right(ResultType.defaultValue(default))

      case ListErrors(_)        => Left(Irrecoverable(List(error)))
      case MapErrors(_)         => Left(Irrecoverable(List(error)))
      case FormatError(_, _, _) => Left(Irrecoverable(List(error)))
      case e                    => Left(Irrecoverable(List(e)))
    }
  }
}
