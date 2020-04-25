package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.char._
import zio.config.ConfigDescriptor._

private[refined] trait CharSupport {

  /** Predicate that checks if a `Char` is a digit */
  def digit[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Digit]
  ): ConfigDescriptor[Refined[A, Digit]] =
    asRefined[A, Digit](desc)

  /** Predicate that checks if a `Char` is a letter */
  def letter[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Letter]
  ): ConfigDescriptor[Refined[A, Letter]] =
    asRefined[A, Letter](desc)

  /** Predicate that checks if a `Char` is a lower case character */
  def lowerCase[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, LowerCase]
  ): ConfigDescriptor[Refined[A, LowerCase]] =
    asRefined[A, LowerCase](desc)

  /** Predicate that checks if a `Char` is an upper case character */
  def upperCase[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, UpperCase]
  ): ConfigDescriptor[Refined[A, UpperCase]] =
    asRefined[A, UpperCase](desc)

  /** Predicate that checks if a `Char` is white space */
  def whitespace[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Whitespace]
  ): ConfigDescriptor[Refined[A, Whitespace]] =
    asRefined[A, Whitespace](desc)

}
