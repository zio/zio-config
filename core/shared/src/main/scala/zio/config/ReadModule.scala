package zio.config

import com.github.ghik.silencer.silent
import zio.{IO, ZIO, ZManaged}
import zio.config.ReadError._

@silent("Unused import")
private[config] trait ReadModule extends ConfigDescriptorModule {
  import VersionSpecificSupport._
  import ConfigSource._

  final def read[A](
    configuration: ConfigDescriptor[A]
  ): IO[ReadError[K], A] = {
    type Res[+B] = ZManaged[Any, ReadError[K], AnnotatedRead[B]]

    import ConfigDescriptorAdt._

    def formatError(paths: List[Step[K]], actualType: String, expectedType: String, descriptions: List[String]) =
      ReadError.FormatError(
        paths.reverse,
        s"Provided value is of type $actualType, expecting the type $expectedType",
        descriptions
      )

    def loopNested[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Nested[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] = {
      val updatedKeys = cfg.path :: keys
      val updatedPath = Step.Key(cfg.path) :: path
      loopAny(updatedPath, updatedKeys, cfg.config, descriptions, programSummary)
    }

    def loopOptional[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Optional[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Option[B]] =
      loopAny(path, keys, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fromEither(handleDefaultValues(error, cfg.config, None))

        case Right(value) =>
          ZManaged.succeed(
            AnnotatedRead(Some(value.value), Set(AnnotatedRead.Annotation.NonDefaultValue) ++ value.annotations)
          )
      }

    def loopDefault[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Default[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      loopAny(path, keys, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fromEither(handleDefaultValues(error, cfg.config, cfg.default))

        case Right(value) =>
          ZManaged.succeed(
            AnnotatedRead(value.value, Set(AnnotatedRead.Annotation.NonDefaultValue) ++ value.annotations)
          )
      }

    def loopOrElse[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: OrElse[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      loopAny(path, keys, cfg.left, descriptions, programSummary).either flatMap {
        case a @ Right(_)    => ZManaged.fromEither(a)
        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions, programSummary).either flatMap {
            case a @ Right(_) =>
              ZManaged.fromEither(a)

            case Left(rightError) =>
              ZManaged.fail(
                ReadError.OrErrors(leftError :: rightError :: Nil, leftError.annotations ++ rightError.annotations)
              )
          }
      }

    def loopOrElseEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: OrElseEither[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Either[B, C]] =
      loopAny(path, keys, cfg.left, descriptions, programSummary).either flatMap {
        case Right(value) =>
          ZManaged.succeed(value.map(Left(_)))

        case Left(leftError) =>
          loopAny(path, keys, cfg.right, descriptions, programSummary).either flatMap {
            case Right(rightValue) =>
              ZManaged.succeed(rightValue.map(Right(_)))

            case Left(rightError) =>
              ZManaged.fail(
                ReadError.OrErrors(leftError :: rightError :: Nil, leftError.annotations ++ rightError.annotations)
              )
          }
      }

    def loopSource[B](path: List[Step[K]], keys: List[K], cfg: Source[B], descriptions: List[String]): Res[B] =
      for {
        maybeMemoized <-
          cfg.source
            .getConfigValue(
              PropertyTreePath(keys.reverse.toVector.map(PropertyTreePath.Step.Value(_)))
            )
        zio           <- maybeMemoized
        tree          <- zio.toManaged_
        res           <- tree match {
                           case PropertyTree.Empty       => ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                           case PropertyTree.Record(_)   => ZManaged.fail(formatError(path, "Record", "Leaf", descriptions))
                           case PropertyTree.Sequence(_) => ZManaged.fail(formatError(path, "Sequence", "Leaf", descriptions))
                           case PropertyTree.Leaf(value) =>
                             cfg.propertyType.read(value) match {
                               case Left(parseError) =>
                                 ZManaged.fail(
                                   formatError(
                                     path.reverse,
                                     parseError.value.toString,
                                     parseError.typeInfo,
                                     descriptions
                                   )
                                 )
                               case Right(parsed)    =>
                                 ZManaged.succeed(AnnotatedRead(parsed, Set.empty))
                             }
                         }
      } yield res

    def loopZip[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: Zip[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[(B, C)] =
      loopAny(path, keys, cfg.left, descriptions, programSummary).either
        .zip(loopAny(path, keys, cfg.right, descriptions, programSummary).either)
        .flatMap {
          case (Right(leftV), Right(rightV)) =>
            ZManaged.succeed(leftV.zip(rightV))

          case (Left(error1), Left(error2)) =>
            ZManaged.fail(ZipErrors(error1 :: error2 :: Nil, error1.annotations ++ error2.annotations))

          case (Left(error), Right(annotated)) =>
            ZManaged.fail(ZipErrors(error :: Nil, error.annotations ++ annotated.annotations))

          case (Right(annotated), Left(error)) =>
            ZManaged.fail(ZipErrors(error :: Nil, error.annotations ++ annotated.annotations))
        }

    def loopXmapEither[B, C](
      path: List[Step[K]],
      keys: List[K],
      cfg: TransformOrFail[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[C] =
      loopAny(path, keys, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fail(error)
        case Right(a)    =>
          ZManaged.fromEither(
            a.mapError(cfg.f).swap.map(message => ReadError.ConversionError(path.reverse, message, a.annotations)).swap
          )
      }

    def loopMap[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: DynamicMap[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Map[K, B]] =
      for {
        maybeMemoized <- cfg.source.getConfigValue(
                           PropertyTreePath(keys.reverse.toVector.map(k => PropertyTreePath.Step.Value(k)))
                         )

        zio  <- maybeMemoized
        tree <- zio.toManaged_
        res  <- tree match {
                  case PropertyTree.Leaf(_)        => ZManaged.fail(formatError(path, "Leaf", "Record", descriptions))
                  case PropertyTree.Sequence(_)    => ZManaged.fail(formatError(path, "Sequence", "Record", descriptions))
                  case PropertyTree.Record(values) =>
                    val result: List[(K, Res[B])] =
                      values.toList.map { case ((k, tree)) =>
                        (
                          k,
                          loopAny(
                            Step.Key(k) :: path,
                            Nil,
                            cfg.config.updateSource(_ => cfg.source.withTree(tree)),
                            descriptions,
                            programSummary
                          )
                        )
                      }

                    seqMap2[K, ReadError[K], B](result.map({ case (a, b) => (a, b.map(_.value)) }).toMap)
                      .mapError(errs => ReadError.MapErrors(errs, errs.flatMap(_.annotations).toSet))
                      .map(mapp => AnnotatedRead(mapp, Set.empty))

                  case PropertyTree.Empty => ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                }
      } yield res

    def loopSequence[B](
      path: List[Step[K]],
      keys: List[K],
      cfg: Sequence[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[List[B]] = {
      def fromTrees(values: List[PropertyTree[K, V]]) = {
        val list = values.zipWithIndex.map { case (tree, idx) =>
          val source =
            cfg.source.withTree(tree)

          loopAny(
            Step.Index(idx) :: path,
            Nil,
            cfg.config.updateSource(_ => source),
            descriptions,
            programSummary
          )
        }

        seqEither2[ReadError[K], B, ReadError[K]]((_, a) => a)(list.map(res => res.map(_.value)))
          .mapError(errs => ReadError.ListErrors(errs, errs.flatMap(_.annotations).toSet))
          .map(list => AnnotatedRead(list, Set.empty))
      }

      for {
        maybeMemoized <-
          cfg.source.getConfigValue(PropertyTreePath(keys.reverse.map(PropertyTreePath.Step.Value(_)).toVector))

        zio  <- maybeMemoized
        tree <- zio.toManaged_
        res  <- tree match {
                  case leaf @ PropertyTree.Leaf(_) =>
                    cfg.source.canSingletonBeSequence match {
                      case LeafForSequence.Invalid => ZManaged.fail(formatError(path, "Leaf", "Sequence", descriptions))
                      case LeafForSequence.Valid   => fromTrees(List(leaf))
                    }

                  case PropertyTree.Record(_)        => ZManaged.fail(formatError(path, "Record", "Sequence", descriptions))
                  case PropertyTree.Empty            => ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                  case PropertyTree.Sequence(values) => fromTrees(values)
                }
      } yield res
    }

    def loopAny[B](
      path: List[Step[K]],
      keys: List[K],
      config: ConfigDescriptor[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      for {
        isEmpty     <- isEmptyConfigSource(config, keys.reverse)
        alreadySeen <- ZManaged.succeed(programSummary.contains(config))
        res         <- if (alreadySeen && isEmpty)
                         ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                       else
                         config match {
                           case c @ Lazy(thunk) =>
                             loopAny(path, keys, thunk(), descriptions, c :: programSummary)

                           case c @ Default(_, _) =>
                             loopDefault(path, keys, c, descriptions, c :: programSummary)

                           case c @ Describe(_, message) =>
                             loopAny(path, keys, c.config, descriptions :+ message, c :: programSummary)

                           case c @ DynamicMap(_, _) =>
                             loopMap(path, keys, c, descriptions, c :: programSummary)

                           case c @ Nested(_, _, _) =>
                             loopNested(path, keys, c, descriptions, c :: programSummary)

                           case c @ Optional(_) =>
                             loopOptional(path, keys, c, descriptions, c :: programSummary)

                           case c @ OrElse(_, _) =>
                             loopOrElse(path, keys, c, descriptions, c :: programSummary)

                           case c @ OrElseEither(_, _) =>
                             loopOrElseEither(path, keys, c, descriptions, c :: programSummary)

                           case c @ Source(_, _) =>
                             loopSource(path, keys, c, descriptions)

                           case c @ Zip(_, _) =>
                             loopZip(path, keys, c, descriptions, c :: programSummary)

                           case c @ TransformOrFail(_, _, _) =>
                             loopXmapEither(path, keys, c, descriptions, c :: programSummary)

                           case c @ Sequence(_, _) =>
                             loopSequence(path, keys, c, descriptions, c :: programSummary)
                         }

      } yield res.asInstanceOf[AnnotatedRead[B]]

    loopAny(Nil, Nil, configuration, Nil, Nil).map(_.value).use(a => ZIO.succeed(a))

  }

  private[config] def isEmptyConfigSource[A](
    config: ConfigDescriptor[A],
    keys: List[K]
  ): ZManaged[Any, ReadError[K], Boolean] = {
    val sourceTrees =
      config.sources.map(_.getConfigValue(PropertyTreePath(keys.toVector.map(PropertyTreePath.Step.Value(_)))))

    ZManaged.forall(sourceTrees) { managed =>
      for {
        memoized <- managed
        treeZIO  <- memoized
        tree     <- treeZIO.toManaged_
      } yield tree.isEmpty
    }
  }

  private[config] def foldReadError[B](
    error: ReadError[K]
  )(alternative: B)(f: PartialFunction[ReadError[K], B])(g: (B, B) => B, zero: B): B = {
    def go(list: List[ReadError[K]]): B =
      list.foldLeft(zero)((a, b) => g(foldReadError(b)(alternative)(f)(g, zero), a))

    error match {
      case e @ ReadError.MissingValue(_, _, _)    => f.applyOrElse(e, (_: ReadError[K]) => alternative)
      case e @ ReadError.SourceError(_, _)        => f.applyOrElse(e, (_: ReadError[K]) => alternative)
      case e @ ReadError.FormatError(_, _, _, _)  => f.applyOrElse(e, (_: ReadError[K]) => alternative)
      case e @ ReadError.ConversionError(_, _, _) => f.applyOrElse(e, (_: ReadError[K]) => alternative)
      case e @ ReadError.Irrecoverable(list, _)   => f.applyOrElse(e, (_: ReadError[K]) => go(list))
      case e @ ReadError.OrErrors(list, _)        => f.applyOrElse(e, (_: ReadError[K]) => go(list))
      case e @ ReadError.ZipErrors(list, _)       => f.applyOrElse(e, (_: ReadError[K]) => go(list))
      case e @ ReadError.ListErrors(list, _)      => f.applyOrElse(e, (_: ReadError[K]) => go(list))
      case e @ ReadError.MapErrors(list, _)       => f.applyOrElse(e, (_: ReadError[K]) => go(list))
    }
  }

  private[config] def handleDefaultValues[A, B](
    error: ReadError[K],
    config: ConfigDescriptor[A],
    default: B
  ): Either[ReadError[K], AnnotatedRead[B]] = {

    val hasOnlyMissingValuesAndZeroIrrecoverable =
      foldReadError(error)(alternative = false) {
        case ReadError.MissingValue(_, _, _) => true
        case ReadError.Irrecoverable(_, _)   => false
      }(_ && _, true)

    val baseConditionForFallBack =
      hasOnlyMissingValuesAndZeroIrrecoverable && sizeOfZipAndOrErrors(error) == requiredZipAndOrFields(config)

    def hasZeroNonDefaultValues(annotations: Set[AnnotatedRead.Annotation]) =
      !annotations.contains(AnnotatedRead.Annotation.NonDefaultValue)

    error match {
      case MissingValue(_, _, annotations) => Right(AnnotatedRead(default, annotations))

      case ReadError.ZipErrors(_, annotations) if baseConditionForFallBack && hasZeroNonDefaultValues(annotations) =>
        Right(AnnotatedRead(default, annotations))

      case ReadError.OrErrors(_, annotations) if baseConditionForFallBack && hasZeroNonDefaultValues(annotations) =>
        Right(AnnotatedRead(default, annotations))

      case e =>
        Left(Irrecoverable(List(e)))
    }
  }

  private[config] def parseErrorMessage(givenValue: String, expectedType: String) =
    s"Provided value is ${givenValue.toString}, expecting the type ${expectedType}"

  final def requiredZipAndOrFields[A](config: ConfigDescriptor[A]): Int = {
    def loop[B](count: List[K], config: ConfigDescriptor[B]): Int =
      config match {
        case ConfigDescriptorAdt.Lazy(thunk)                => loop(count, thunk())
        case ConfigDescriptorAdt.Zip(left, right)           => loop(count, left) + loop(count, right)
        case ConfigDescriptorAdt.TransformOrFail(cfg, _, _) => loop(count, cfg)
        case ConfigDescriptorAdt.Describe(cfg, _)           => loop(count, cfg)
        case ConfigDescriptorAdt.Nested(_, _, next)         => loop(count, next)
        case ConfigDescriptorAdt.Source(_, _)               => 1
        case ConfigDescriptorAdt.Optional(_)                => 0
        case ConfigDescriptorAdt.OrElse(left, right)        => loop(count, left) + loop(count, right)
        case ConfigDescriptorAdt.OrElseEither(left, right)  => loop(count, left) + loop(count, right)
        case ConfigDescriptorAdt.Default(_, _)              => 0
        case ConfigDescriptorAdt.Sequence(_, _)             => 1
        case ConfigDescriptorAdt.DynamicMap(_, _)           => 1
      }

    loop(Nil, config)
  }

  def sizeOfZipAndOrErrors(error: ReadError[K]): Int =
    foldReadError(error)(0) {
      case ReadError.ListErrors(_, _)         => 1
      case ReadError.MapErrors(_, _)          => 1
      case ReadError.Irrecoverable(_, _)      => 1
      case ReadError.MissingValue(_, _, _)    => 1
      case ReadError.FormatError(_, _, _, _)  => 1
      case ReadError.ConversionError(_, _, _) => 1
    }(_ + _, 0)

}
