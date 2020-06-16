package zio.config.derivation

import scala.annotation.implicitAmbiguous

/**
 * Preventing derivation for List, Option and Either.
 * */
sealed trait NeedsDerive[+T]

object NeedsDerive extends NeedsDerive[Nothing] {

  implicit def needsDerive[R]: NeedsDerive[R] = NeedsDerive

  @implicitAmbiguous(
    "Can't derive ConfigDescriptor for `List[T]` directly." +
      " Wrap it with a `case class Config(list: List[T])` or use `list(descriptor[T])` manually."
  )
  implicit def needsDeriveAmbiguousList1: NeedsDerive[List[Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousList2: NeedsDerive[List[Nothing]] = NeedsDerive

  @implicitAmbiguous(
    "Can't derive ConfigDescriptor for `Option[T]` directly." +
      " Wrap it with a `case class Config(list: Option[T])` or use `descriptor[T].optional` manually."
  )
  implicit def needsDeriveAmbiguousOption1: NeedsDerive[Option[Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousOption2: NeedsDerive[Option[Nothing]] = NeedsDerive

  @implicitAmbiguous(
    "Can't derive ConfigDescriptor for `Either[A, B]` directly." +
      " Wrap it with a `case class Config(list: Either[A, B])`" +
      " or use `descriptor[A].orElseEither(descriptor[B])` manually."
  )
  implicit def needsDeriveAmbiguousEither1: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive
  implicit def needsDeriveAmbiguousEither2: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive
}
