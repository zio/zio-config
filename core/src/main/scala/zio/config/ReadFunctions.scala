package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ReadError.{ AndErrors, OrErrors, Step }
import zio.config.ReadFunctions._

private[config] trait ReadFunctions {
  final def read[A](
    configuration: ConfigDescriptor[A]
  ): Either[ReadError, A] = {

    type Res[+B] = Either[ReadError, B]

    def formatError(paths: List[Step], actualType: String, expectedType: String) =
      Left(
        ReadError.FormatError(paths.reverse, s"Provided value is of type $actualType, expecting the type $expectedType")
      )

    def loopDefault[B](path: List[Step], keys: List[String], cfg: Default[B]): Res[B] =
      loopAny(path, keys, cfg.config) match {
        case Left(error) if hasParseErrors(error) || hasConversionErrors(error) => Left(error)
        case Left(_)                                                            => Right(cfg.value)
        case Right(value)                                                       => Right(value)
      }

    def loopDescribe[B](path: List[Step], keys: List[String], cfg: Describe[B]): Res[B] =
      loopAny(path, keys, cfg.config)

    def loopNested[B](path: List[Step], keys: List[String], cfg: Nested[B]): Res[B] =
      loopAny(Step.Key(cfg.path) :: path, cfg.path :: keys, cfg.config)

    def loopOptional[B](path: List[Step], keys: List[String], cfg: Optional[B]): Res[Option[B]] =
      loopAny(path, keys, cfg.config) match {
        case Left(error) if hasParseErrors(error) || hasConversionErrors(error) => Left(error)
        case Left(_)                                                            => Right(None)
        case Right(value)                                                       => Right(Some(value))
      }

    def loopOrElse[B](path: List[Step], keys: List[String], cfg: OrElse[B]): Res[B] =
      loopAny(path, keys, cfg.left) match {
        case Right(value) => Right(value)
        case Left(leftError) =>
          loopAny(path, keys, cfg.right) match {
            case Right(rightValue) => Right(rightValue)
            case Left(rightError)  => Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopOrElseEither[B, C](path: List[Step], keys: List[String], cfg: OrElseEither[B, C]): Res[Either[B, C]] =
      loopAny(path, keys, cfg.left) match {
        case Right(value) => Right(Left(value))
        case Left(leftError) =>
          loopAny(path, keys, cfg.right) match {
            case Right(rightValue) => Right(Right(rightValue))
            case Left(rightError)  => Left(ReadError.OrErrors(leftError :: rightError :: Nil))
          }
      }

    def loopSource[B](path: List[Step], keys: List[String], cfg: Source[B]): Res[B] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Empty       => Left(ReadError.MissingValue(path.reverse))
        case PropertyTree.Record(_)   => formatError(path, "Record", "Leaf")
        case PropertyTree.Sequence(_) => formatError(path, "Sequence", "Leaf")
        case PropertyTree.Leaf(value) =>
          cfg.propertyType.read(value) match {
            case Left(parseError) =>
              Left(
                ReadError.FormatError(
                  path.reverse,
                  ReadFunctions.parseErrorMessage(parseError.value.toString, parseError.typeInfo)
                )
              )
            case Right(parsed) => Right(parsed)
          }
      }

    def loopZip[B, C](path: List[Step], keys: List[String], cfg: Zip[B, C]): Res[(B, C)] =
      (loopAny(path, keys, cfg.left), loopAny(path, keys, cfg.right)) match {
        case (Right(leftV), Right(rightV)) => Right((leftV, rightV))
        case (Left(leftE), Left(rightE))   => Left(AndErrors(leftE :: rightE :: Nil))
        case (Left(leftE), _)              => Left(leftE)
        case (_, Left(rightE))             => Left(rightE)
      }

    def loopXmapEither[B, C](path: List[Step], keys: List[String], cfg: XmapEither[B, C]): Res[C] =
      loopAny(path, keys, cfg.config) match {
        case Left(error) => Left(error)
        case Right(a)    => cfg.f(a).swap.map(ReadError.ConversionError(path.reverse, _)).swap
      }

    def loopSequence[B](path: List[Step], keys: List[String], cfg: Sequence[B]): Res[List[B]] =
      cfg.source.getConfigValue(keys.reverse) match {
        case PropertyTree.Leaf(_)   => formatError(path, "Leaf", "Sequence")
        case PropertyTree.Record(_) => formatError(path, "Record", "Sequence")
        case PropertyTree.Empty     => Left(ReadError.MissingValue(path.reverse))
        case PropertyTree.Sequence(values) =>
          val list = values.zipWithIndex.map {
            case (tree, idx) =>
              val source = ConfigSource(tree, cfg.source.sourceDescription)
              loopAny(Step.Index(idx) :: path, Nil, cfg.config.updateSource(_ => source))
          }
          seqEither2[ReadError, B, ReadError]((_, a) => a)(list).swap.map(AndErrors(_)).swap
      }

    def loopAny[B](path: List[Step], keys: List[String], config: ConfigDescriptor[B]): Res[B] =
      config match {
        case c @ Default(_, _)       => loopDefault(path, keys, c)
        case c @ Describe(_, _)      => loopDescribe(path, keys, c)
        case c @ Nested(_, _)        => loopNested(path, keys, c)
        case c @ Optional(_)         => loopOptional(path, keys, c)
        case c @ OrElse(_, _)        => loopOrElse(path, keys, c)
        case c @ OrElseEither(_, _)  => loopOrElseEither(path, keys, c)
        case c @ Source(_, _)        => loopSource(path, keys, c)
        case c @ Zip(_, _)           => loopZip(path, keys, c)
        case c @ XmapEither(_, _, _) => loopXmapEither(path, keys, c)
        case c @ Sequence(_, _)      => loopSequence(path, keys, c)
      }

    loopAny(Nil, Nil, configuration)
  }
}

object ReadFunctions {

  def parseErrorMessage(given: String, expectedType: String) =
    s"Provided value is ${given.toString}, expecting the type ${expectedType}"

  private def hasErrors[K](value: ReadError)(f: PartialFunction[ReadError, Boolean]): Boolean =
    f.orElse[ReadError, Boolean]({
        case OrErrors(errors)  => errors.exists(hasErrors(_)(f))
        case AndErrors(errors) => errors.exists(hasErrors(_)(f))
      })
      .lift(value)
      .getOrElse(false)

  private def hasParseErrors[K](error: ReadError): Boolean =
    hasErrors(error)({ case ReadError.FormatError(_, _) => true })

  private def hasConversionErrors[K](error: ReadError): Boolean =
    hasErrors(error)({ case ReadError.ConversionError(_, _) => true })
}
