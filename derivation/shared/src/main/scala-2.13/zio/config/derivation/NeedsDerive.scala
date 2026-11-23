package zio.config.derivation

import scala.annotation.implicitAmbiguous

/**
 * Compile-time evidence that `T` is not a `List`, `Option` or `Either`, nor a supertype of one. An implicit
 * `NeedsDerive[T]` resolves for a type such as `Int` or `Vector[Int]`, while for `List[A]`, `Option[A]` and
 * `Either[A, B]` implicit search fails with an ambiguity error whose message suggests wrapping the value in a case
 * class. Because `NeedsDerive` is covariant, the search also fails for a supertype of these, such as `Seq[Int]`,
 * `Iterable[String]` or `Product`.
 *
 * ZIO Config's derivation (`deriveConfig`) does not ask for it, so `deriveConfig[List[Int]]` compiles. Some of the
 * messages suggest `descriptor[T]`, a function that ZIO Config 4.x doesn't have; use `deriveConfig[T]`.
 */
sealed trait NeedsDerive[+T]

/** The single `NeedsDerive` value, and the implicits that provide or refuse it. */
object NeedsDerive extends NeedsDerive[Nothing] {

  /**
   * Provides `NeedsDerive[R]` for a type `R` that is not a `List`, `Option` or `Either`, nor a supertype of one such as
   * `Seq[Int]`.
   */
  implicit def needsDerive[R]: NeedsDerive[R] = NeedsDerive

  /**
   * One of two identical implicits that make `NeedsDerive[List[A]]` ambiguous; implicit search then fails with this
   * annotation's message.
   */
  @implicitAmbiguous(
    "Can't derive Config for `List[T]` directly." +
      " Wrap it with a `case class Config(list: List[T])` or use `listOf(descriptor[T])` manually."
  )
  implicit def needsDeriveAmbiguousList1: NeedsDerive[List[Nothing]] = NeedsDerive

  /** The second of the two identical implicits that make `NeedsDerive[List[A]]` ambiguous. */
  implicit def needsDeriveAmbiguousList2: NeedsDerive[List[Nothing]] = NeedsDerive

  /**
   * One of two identical implicits that make `NeedsDerive[Option[A]]` ambiguous; implicit search then fails with this
   * annotation's message.
   */
  @implicitAmbiguous(
    "Can't derive Config for `Option[T]` directly." +
      " Wrap it with a `case class Config(list: Option[T])` or use `descriptor[T].optional` manually."
  )
  implicit def needsDeriveAmbiguousOption1: NeedsDerive[Option[Nothing]] = NeedsDerive

  /** The second of the two identical implicits that make `NeedsDerive[Option[A]]` ambiguous. */
  implicit def needsDeriveAmbiguousOption2: NeedsDerive[Option[Nothing]] = NeedsDerive

  /**
   * One of two identical implicits that make `NeedsDerive[Either[A, B]]` ambiguous; implicit search then fails with
   * this annotation's message.
   */
  @implicitAmbiguous(
    "Can't derive Config for `Either[A, B]` directly." +
      " Wrap it with a `case class Config(list: Either[A, B])`" +
      " or use `descriptor[A].orElseEither(descriptor[B])` manually."
  )
  implicit def needsDeriveAmbiguousEither1: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive

  /** The second of the two identical implicits that make `NeedsDerive[Either[A, B]]` ambiguous. */
  implicit def needsDeriveAmbiguousEither2: NeedsDerive[Either[Nothing, Nothing]] = NeedsDerive
}
