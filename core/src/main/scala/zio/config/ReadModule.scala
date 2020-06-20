package zio.config

import zio.config.ReadError.{ AndErrors, MissingValue, OrErrors, Step }
import VersionSpecificSupport._

// (string("name") |@| int("age").optional)(Person.apply, Person.unapply)
private[config] trait ReadModule extends ConfigDescriptorModule {
  final def read[A](
    configuration: ConfigDescriptor[A]
  ): Either[ReadError[K], A] = {
    type Res[+B] = Either[ReadError[K], B]

    import ConfigDescriptorAdt._

    def formatError(paths: List[Step[K]], actualType: String, expectedType: String) =
      Left(
        ReadError.FormatError(
          paths.reverse,
          s"Provided value is of type $actualType, expecting the type $expectedType"
        )
      )

    def loopDefault[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Default[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[B] =
      loopAny(path, keys, cfg.config, descriptions, optionalParentPath) match {
        case Left(error) if hasUnrecoverableErrors(error) => Left(error)
        case Left(_)                                      => Right(cfg.value)
        case Right(value)                                 => Right(value)
      }

    def loopOptional[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Optional[B],
      descriptions: List[String]
    ): Res[Option[B]] =
      loopAny(path, keys, cfg.config, descriptions, keys) match {
        case Left(error) if hasUnrecoverableErrors(error) =>
          Left(error)
        case Left(_)      => Right(None)
        case Right(value) => Right(Some(value))
      }

    def loopOrElse[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: OrElse[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[B] =
      loopAny(path, keys, cfg.left, descriptions, optionalParentPath) match {
        case Right(value) => Right(value)
        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions, optionalParentPath) match {
            case Right(rightValue) => Right(rightValue)
            case Left(rightError) =>
              Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopOrElseEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: OrElseEither[B, C],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[Either[B, C]] =
      loopAny(path, keys, cfg.left, descriptions, optionalParentPath) match {
        case Right(value) => Right(Left(value))
        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions, optionalParentPath) match {
            case Right(rightValue) => Right(Right(rightValue))
            case Left(rightError) =>
              Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopSource[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Source[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[B] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Empty =>
          handleParentOptionalPath(
            optionalParentPath,
            ReadError.MissingValue(path.reverse, descriptions),
            cfg.source.getConfigValue
          )

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
            case Right(parsed) => Right(parsed)
          }
      }

    def loopZip[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: Zip[B, C],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[(B, C)] =
      (
        loopAny(path, keys, cfg.left, descriptions, optionalParentPath),
        loopAny(path, keys, cfg.right, descriptions, optionalParentPath)
      ) match {
        case (Right(leftV), Right(rightV)) => Right((leftV, rightV))
        case (Left(leftE), Left(rightE))   => Left(AndErrors(leftE :: rightE :: Nil))
        case (Left(leftE), _)              => Left(ReadError.ForceSeverity(leftE, false))
        case (_, Left(rightE))             => Left(ReadError.ForceSeverity(rightE, false))
      }

    def loopXmapEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: XmapEither[B, C],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[C] =
      loopAny(path, keys, cfg.config, descriptions, optionalParentPath) match {
        case Left(error) => Left(error)
        case Right(a) =>
          cfg.f(a).swap.map(ReadError.ConversionError(path.reverse, _)).swap
      }

    def loopMap[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: DynamicMap[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[Map[K, B]] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Leaf(_)     => formatError(path, "Leaf", "Record")
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Record")
        case PropertyTree.Record(values) =>
          val result: List[(K, Res[B])] = values.toList.zipWithIndex.map {
            case ((k, tree), _) =>
              val source: ConfigSource =
                getConfigSource(cfg.source.names, tree.getPath)

              (k, loopAny(path, Nil, cfg.config.updateSource(_ => source), descriptions, optionalParentPath))
          }

          seqMap2[K, ReadError[K], B, ReadError[K]]((_, k, error) => error.atKey(k))(result.toMap).swap
            .map(errs => ReadError.ForceSeverity(AndErrors(errs), treatAsMissing = false))
            .swap

        case PropertyTree.Empty =>
          handleParentOptionalPath(
            optionalParentPath,
            ReadError.MissingValue(path.reverse, descriptions),
            cfg.source.getConfigValue
          )
      }

    def loopSequence[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Sequence[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[List[B]] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Leaf(_)   => formatError(path, "Leaf", "Sequence")
        case PropertyTree.Record(_) => formatError(path, "Record", "Sequence")
        case PropertyTree.Empty =>
          handleParentOptionalPath(
            optionalParentPath,
            ReadError.MissingValue(path.reverse, descriptions),
            cfg.source.getConfigValue
          )

        case PropertyTree.Sequence(values) =>
          val list = values.zipWithIndex.map {
            case (tree, idx) =>
              val source =
                getConfigSource(cfg.source.names, tree.getPath)
              loopAny(
                Step.Index(idx) :: path,
                Nil,
                cfg.config.updateSource(_ => source),
                descriptions,
                optionalParentPath
              )
          }
          seqEither2[ReadError[K], B, ReadError[K]]((_, a) => a)(list).swap
            .map(errs => ReadError.ForceSeverity(AndErrors(errs), treatAsMissing = false))
            .swap
      }

    def loopAny[B](
      path: List[Step[K]],
      keys: List[K],
      config: ConfigDescriptor[B],
      descriptions: List[String],
      optionalParentPath: List[K]
    ): Res[B] =
      config match {
        case c @ Default(_, _)        => loopDefault(path, keys, c, descriptions, optionalParentPath)
        case c @ Describe(_, message) => loopAny(path, keys, c.config, descriptions :+ message, optionalParentPath)
        case c @ DynamicMap(_, _)     => loopMap(path, keys, c, descriptions, optionalParentPath)
        case c @ Nested(_, _) =>
          loopAny(
            Step.Key(c.path) :: path,
            c.path :: keys,
            c.config,
            descriptions,
            if (optionalParentPath.isEmpty) Nil else optionalParentPath :+ c.path
          )

        case c @ Optional(_)         => loopOptional(path, keys, c, descriptions)
        case c @ OrElse(_, _)        => loopOrElse(path, keys, c, descriptions, optionalParentPath)
        case c @ OrElseEither(_, _)  => loopOrElseEither(path, keys, c, descriptions, optionalParentPath)
        case c @ Source(_, _)        => loopSource(path, keys, c, descriptions, optionalParentPath)
        case c @ Zip(_, _)           => loopZip(path, keys, c, descriptions, optionalParentPath)
        case c @ XmapEither(_, _, _) => loopXmapEither(path, keys, c, descriptions, optionalParentPath)
        case c @ Sequence(_, _)      => loopSequence(path, keys, c, descriptions, optionalParentPath)
      }

    loopAny(Nil, Nil, configuration, Nil, Nil)
  }

  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  private def hasUnrecoverableErrors[K](
    value: ReadError[K]
  ): Boolean = value match {
    case ReadError.ForceSeverity(_, treatAsMissing) => !treatAsMissing
    case ReadError.FormatError(_, _)                => true
    case ReadError.ConversionError(_, _)            => true
    case OrErrors(errors)                           => errors.exists(hasUnrecoverableErrors)
    case AndErrors(errors)                          => errors.exists(hasUnrecoverableErrors)
    case MissingValue(_, _)                         => false
  }

  private def handleParentOptionalPath(
    optionalParentPath: List[K],
    readError: ReadError[K],
    f: List[K] => PropertyTree[K, V]
  ) =
    optionalParentPath match {
      case ::(_, _) =>
        f(optionalParentPath) match {
          case PropertyTree.Leaf(_)     => Left(ReadError.ForceSeverity(readError, false))
          case PropertyTree.Record(_)   => Left(ReadError.ForceSeverity(readError, false))
          case PropertyTree.Empty       => Left(readError)
          case PropertyTree.Sequence(_) => Left(ReadError.ForceSeverity(readError, false))
        }
      case Nil =>
        Left(readError)
    }
}
