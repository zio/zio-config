package zio.config.refined

import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.char._
import zio.config.ConfigDescriptor

private[refined] trait CharSupport {

  /** Predicate that checks if a `Char` is a digit */
  def digit[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Digit]
  ): ConfigDescriptor[K, V, Refined[A, Digit]] =
    asRefined[K, V, A, Digit](desc)

  /** Predicate that checks if a `Char` is a letter */
  def letter[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Letter]
  ): ConfigDescriptor[K, V, Refined[A, Letter]] =
    asRefined[K, V, A, Letter](desc)

  /** Predicate that checks if a `Char` is a lower case character */
  def lowerCase[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, LowerCase]
  ): ConfigDescriptor[K, V, Refined[A, LowerCase]] =
    asRefined[K, V, A, LowerCase](desc)

  /** Predicate that checks if a `Char` is an upper case character */
  def upperCase[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, UpperCase]
  ): ConfigDescriptor[K, V, Refined[A, UpperCase]] =
    asRefined[K, V, A, UpperCase](desc)

  /** Predicate that checks if a `Char` is white space */
  def whitespace[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Whitespace]
  ): ConfigDescriptor[K, V, Refined[A, Whitespace]] =
    asRefined[K, V, A, Whitespace](desc)

}
