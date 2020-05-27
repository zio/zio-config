package zio.config.derivation

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Can't derive ConfigDescriptor for `List[T]`, `Option[T]` or `Either[A, B]` directly." +
    " Wrap it with a `case class Config(list: List[T])` or use `list(descriptor[T])` manually."
)
sealed trait NeedsDerive[+T]

object NeedsDerive extends NeedsDerive[Nothing] {

  implicit def needsDerive[R]: NeedsDerive[R] = NeedsDerive

  implicit def needsDeriveAmbiguousList1: NeedsDerive[List[Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousList2: NeedsDerive[List[Nothing]] = NeedsDerive

  implicit def needsDeriveAmbiguousOption1: NeedsDerive[Option[Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousOption2: NeedsDerive[Option[Nothing]] = NeedsDerive

  implicit def needsDeriveAmbiguousEither1: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousEither2: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive
}
