package zio.config

import com.github.ghik.silencer.silent
import zio.config.ReadError._
import zio.{IO, ZIO, ZManaged}

import scala.collection.mutable.{Map => MutableMap}

import PropertyTreePath.Step

@silent("Unused import")
private[config] trait ReadModule extends ConfigDescriptorModule {
  import VersionSpecificSupport._

  type CachedReaders = MutableMap[ConfigSource, ConfigSource.ManagedReader]

  final def read[A](
    configuration: ConfigDescriptor[A]
  ): IO[ReadError[K], A] = {
    type Res[+B] = ZManaged[Any, ReadError[K], AnnotatedRead[PropertyTree[K, B]]]

    val cachedSources: CachedReaders = MutableMap()

    import ConfigDescriptorAdt._

    def formatError(paths: List[Step[K]], actual: String, expected: String, descriptions: List[String]) =
      ReadError.FormatError(
        paths.reverse,
        s"Provided value is $actual, expecting the type $expected",
        descriptions
      )

    def loopNested[B](
      path: List[Step[K]],
      cfg: Nested[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      loopAny(path, cfg.config, descriptions, programSummary)

    def loopOptional[B](
      path: List[Step[K]],
      cfg: Optional[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Option[B]] =
      loopAny(path, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fromEither(handleDefaultValues(error, cfg.config, None))

        case Right(value) =>
          ZManaged.succeed(
            AnnotatedRead(value.value.map(Some(_)), Set(AnnotatedRead.Annotation.NonDefaultValue) ++ value.annotations)
          )
      }

    def loopDefault[B](
      path: List[Step[K]],
      cfg: Default[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      loopAny(path, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fromEither(handleDefaultValues(error, cfg.config, cfg.default))

        case Right(value) =>
          ZManaged.succeed(
            AnnotatedRead(value.value, Set(AnnotatedRead.Annotation.NonDefaultValue) ++ value.annotations)
          )
      }

    def loopOrElse[B](
      path: List[Step[K]],
      cfg: OrElse[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      loopAny(path, cfg.left, descriptions, programSummary).either flatMap {
        case a @ Right(_) =>
          ZManaged.fromEither(a)

        case Left(leftError) =>
          loopAny(path, cfg.right, descriptions, programSummary).either flatMap {
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
      cfg: OrElseEither[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Either[B, C]] =
      loopAny(path, cfg.left, descriptions, programSummary).either flatMap {
        case Right(value) =>
          ZManaged.succeed(value.map(_.map(Left(_))))

        case Left(leftError) =>
          loopAny(path, cfg.right, descriptions, programSummary).either flatMap {
            case Right(rightValue) =>
              ZManaged.succeed(rightValue.map(_.map(Right(_))))

            case Left(rightError) =>
              ZManaged.fail(
                ReadError.OrErrors(leftError :: rightError :: Nil, leftError.annotations ++ rightError.annotations)
              )
          }
      }

    def loopSource[B](
      path: List[Step[K]],
      cfg: Source[B],
      descriptions: List[String]
    ): Res[B] =
      for {
        maybeMemoizedReader <- cachedSources.get(cfg.source) match {
                                 case Some(value) =>
                                   ZManaged.succeed(value)
                                 case None        =>
                                   cfg.source.run.access
                               }
        _                   <- ZManaged.succeed(cachedSources.update(cfg.source, maybeMemoizedReader))
        tree                <- ZManaged.fromZIO(maybeMemoizedReader.use(_(PropertyTreePath(path.reverse.toVector))))
        res                 <- tree match {
                                 case PropertyTree.Empty          =>
                                   ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                                 case PropertyTree.Record(_)      =>
                                   ZManaged.fail(formatError(path, "of type Map", "Singleton", descriptions))
                                 case PropertyTree.Sequence(_)    =>
                                   ZManaged.fail(formatError(path, "of type List", "Singleton", descriptions))
                                 case PropertyTree.Leaf(value, _) =>
                                   cfg.propertyType.read(value) match {
                                     case Left(parseError) =>
                                       ZManaged.fail(
                                         formatError(
                                           path,
                                           parseError.value,
                                           parseError.typeInfo,
                                           descriptions
                                         )
                                       )
                                     case Right(parsed)    =>
                                       ZManaged.succeed(AnnotatedRead(PropertyTree.Leaf(parsed), Set.empty))
                                   }
                               }
      } yield res

    def loopZip[B, C](
      path: List[Step[K]],
      cfg: Zip[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[(B, C)] =
      loopAny(path, cfg.left, descriptions, programSummary).either
        .zip(loopAny(path, cfg.right, descriptions, programSummary).either)
        .flatMap {
          case (Right(leftV), Right(rightV)) =>
            ZManaged.succeed(leftV.zipWith(rightV)(_.zip(_)))

          case (Left(error1), Left(error2)) =>
            ZManaged.fail(ZipErrors(error1 :: error2 :: Nil, error1.annotations ++ error2.annotations))

          case (Left(error), Right(annotated)) =>
            ZManaged.fail(ZipErrors(error :: Nil, error.annotations ++ annotated.annotations))

          case (Right(annotated), Left(error)) =>
            ZManaged.fail(ZipErrors(error :: Nil, error.annotations ++ annotated.annotations))
        }

    def loopXmapEither[B, C](
      path: List[Step[K]],
      cfg: TransformOrFail[B, C],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[C] =
      loopAny(path, cfg.config, descriptions, programSummary).either flatMap {
        case Left(error) =>
          ZManaged.fail(error)
        case Right(a)    =>
          ZManaged.fromEither(
            a.mapEither(tree => tree.mapEither(cfg.f))
              .swap
              .map(message => ReadError.ConversionError(path.reverse, message, a.annotations))
              .swap
          )
      }

    def loopMap[B](
      path: List[Step[K]],
      cfg: DynamicMap[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[Map[K, B]] =
      for {
        tree_ <- treeOf(cfg, cachedSources)
        res   <- tree_.at(PropertyTreePath(path.reverse.toVector)) match {
                   case PropertyTree.Leaf(_, _)     =>
                     ZManaged.fail(formatError(path, "of type Singleton", "Map", descriptions))
                   case PropertyTree.Sequence(_)    =>
                     ZManaged.fail(formatError(path, "of type List", "Map", descriptions))
                   case PropertyTree.Record(values) =>
                     val result: List[(K, Res[B])] =
                       values.toList.map { case ((k, _)) =>
                         (
                           k,
                           loopAny(
                             Step.Key(k) :: path,
                             cfg.config,
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
      cfg: Sequence[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[List[B]] = {

      def fromTrees(
        values: List[PropertyTree[K, V]]
      ): ZManaged[Any, ReadError[String], AnnotatedRead[PropertyTree[K, List[B]]]] = {
        val list = values.zipWithIndex.map { case (_, idx) =>
          loopAny(
            Step.Index(idx) :: path,
            cfg.config,
            descriptions,
            programSummary
          )
        }

        seqEither2[K, ReadError[K], B, ReadError[K]]((_, a) => a)(list.map(res => res.map(_.value)))
          .mapError(errs => ReadError.ListErrors(errs, errs.flatMap(_.annotations).toSet))
          .map(list => AnnotatedRead(list, Set.empty))
      }

      for {
        tree_ <- treeOf(cfg, cachedSources)
        res   <- tree_.at(PropertyTreePath(path.reverse.toVector)) match {
                   case leaf @ PropertyTree.Leaf(_, _) => fromTrees(List(leaf))
                   case PropertyTree.Record(_)         => ZManaged.fail(formatError(path, "of type Map", "List", descriptions))
                   case PropertyTree.Empty             => ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                   case PropertyTree.Sequence(values)  => fromTrees(values)
                 }
      } yield res
    }

    def loopAny[B](
      path: List[Step[K]],
      config: ConfigDescriptor[B],
      descriptions: List[String],
      programSummary: List[ConfigDescriptor[_]]
    ): Res[B] =
      for {
        alreadySeen         <- ZManaged.succeed(programSummary.contains(config))
        isEmptyConfigSource <-
          if (alreadySeen) isEmptyConfigSource(config, path.reverse, cachedSources)
          else ZManaged.succeed(false)
        res                 <- if (isEmptyConfigSource) {
                                 ZManaged.fail(ReadError.MissingValue(path.reverse, descriptions))
                               } else
                                 config match {
                                   case c @ Lazy(thunk) =>
                                     loopAny(path, thunk(), descriptions, c :: programSummary)

                                   case c @ Default(_, _) =>
                                     loopDefault(path, c, descriptions, c :: programSummary)

                                   case c @ Describe(_, message) =>
                                     loopAny(path, c.config, descriptions :+ message, c :: programSummary)

                                   case c @ DynamicMap(_) =>
                                     loopMap(path, c, descriptions, c :: programSummary)

                                   case c @ Nested(key, _) =>
                                     loopNested(Step.Key(key) :: path, c, descriptions, c :: programSummary)

                                   case c @ Optional(_) =>
                                     loopOptional(path, c, descriptions, c :: programSummary)

                                   case c @ OrElse(_, _) =>
                                     loopOrElse(path, c, descriptions, c :: programSummary)

                                   case c @ OrElseEither(_, _) =>
                                     loopOrElseEither(path, c, descriptions, c :: programSummary)

                                   case c @ Source(_, _) =>
                                     loopSource(
                                       path,
                                       c,
                                       descriptions
                                     )

                                   case c @ Zip(_, _) =>
                                     loopZip(path, c, descriptions, c :: programSummary)

                                   case c @ TransformOrFail(_, _, _) =>
                                     loopXmapEither(path, c, descriptions, c :: programSummary)

                                   case c @ Sequence(_) =>
                                     loopSequence(path, c, descriptions, c :: programSummary)
                                 }

      } yield res.asInstanceOf[AnnotatedRead[PropertyTree[K, B]]]

    loopAny(Nil, configuration, Nil, Nil)
      .map(_.value)
      .use {
        case PropertyTree.Leaf(value, _) => ZIO.succeed(value)
        case _                           => ZIO.fail(ReadError.SourceError("Failed to read"))
      }

  }

  private[config] def treeOf[A](
    config: ConfigDescriptor[A],
    cachedSources: CachedReaders
  ): ZManaged[Any, ReadError[String], PropertyTree[K, V]] = {
    val sourceTrees = ZManaged.foreach(config.sources.toList) { managed =>
      for {
        existing      <- ZManaged.succeed(cachedSources.get(managed))
        managedReader <- existing match {
                           case Some(value) =>
                             ZManaged.succeed(value)
                           case None        =>
                             managed.run.access
                         }
        rootTree      <- ZManaged.fromZIO(managedReader.use(reader => reader(PropertyTreePath(Vector.empty))))
      } yield rootTree
    }

    sourceTrees.map(_.reduceLeftOption(_.getOrElse(_)).getOrElse(PropertyTree.empty))
  }

  private[config] def isEmptyConfigSource[A](
    config: ConfigDescriptor[A],
    keys: List[Step[K]],
    cachedSources: CachedReaders
  ): ZManaged[Any, ReadError[K], Boolean] =
    ZManaged.forall(config.sources) { managed =>
      for {
        existing      <- ZManaged.succeed(cachedSources.get(managed))
        managedReader <- existing match {
                           case Some(value) =>
                             ZManaged.succeed(value)
                           case None        =>
                             managed.run.access
                         }
        tree          <- ZManaged.fromZIO(managedReader.use(_(PropertyTreePath(keys.toVector))))
      } yield tree == PropertyTree.empty
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
  ): Either[ReadError[K], AnnotatedRead[PropertyTree[K, B]]] = {

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
      case MissingValue(_, _, annotations) => Right(AnnotatedRead(PropertyTree.Leaf(default), annotations))

      case ReadError.ZipErrors(_, annotations) if baseConditionForFallBack && hasZeroNonDefaultValues(annotations) =>
        Right(AnnotatedRead(PropertyTree.Leaf(default), annotations))

      case ReadError.OrErrors(_, annotations) if baseConditionForFallBack && hasZeroNonDefaultValues(annotations) =>
        Right(AnnotatedRead(PropertyTree.Leaf(default), annotations))

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
        case ConfigDescriptorAdt.Nested(_, next)            => loop(count, next)
        case ConfigDescriptorAdt.Source(_, _)               => 1
        case ConfigDescriptorAdt.Optional(_)                => 0
        case ConfigDescriptorAdt.OrElse(left, right)        => loop(count, left) + loop(count, right)
        case ConfigDescriptorAdt.OrElseEither(left, right)  => loop(count, left) + loop(count, right)
        case ConfigDescriptorAdt.Default(_, _)              => 0
        case ConfigDescriptorAdt.Sequence(_)                => 1
        case ConfigDescriptorAdt.DynamicMap(_)              => 1
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
