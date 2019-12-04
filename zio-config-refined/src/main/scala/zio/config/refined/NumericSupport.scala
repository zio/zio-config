package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.numeric._
import zio.config.ConfigDescriptor

private[refined] trait NumericSupport {

  /** Predicate that checks if a numeric value is less than `N` */
  def less[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Less[N]]
  ): ConfigDescriptor[K, V, Refined[A, Less[N]]] =
    asRefined[K, V, A, Less[N]](desc)

  /** Predicate that checks if a numeric value is greater than `N` */
  def greater[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Greater[N]]
  ): ConfigDescriptor[K, V, Refined[A, Greater[N]]] =
    asRefined[K, V, A, Greater[N]](desc)

  /** Predicate that checks if a floating-point number value is not NaN */
  def nonNaN[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonNaN]
  ): ConfigDescriptor[K, V, Refined[A, NonNaN]] =
    asRefined[K, V, A, NonNaN](desc)

  /** Predicate that checks if a numeric value is less than or equal to `N` */
  def lessEqual[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, LessEqual[N]]
  ): ConfigDescriptor[K, V, Refined[A, LessEqual[N]]] =
    asRefined[K, V, A, LessEqual[N]](desc)

  /** Predicate that checks if a numeric value is greater than or equal to `N` */
  def greaterEqual[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, GreaterEqual[N]]
  ): ConfigDescriptor[K, V, Refined[A, GreaterEqual[N]]] =
    asRefined[K, V, A, GreaterEqual[N]](desc)

  /** Predicate that checks if a numeric value is positive (> 0) */
  def positive[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Positive]
  ): ConfigDescriptor[K, V, Refined[A, Positive]] =
    asRefined[K, V, A, Positive](desc)

  /** Predicate that checks if a numeric value is zero or negative (<= 0) */
  def nonPositive[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonPositive]
  ): ConfigDescriptor[K, V, Refined[A, NonPositive]] =
    asRefined[K, V, A, NonPositive](desc)

  /** Predicate that checks if a numeric value is negative (< 0) */
  def negative[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Negative]
  ): ConfigDescriptor[K, V, Refined[A, Negative]] =
    asRefined[K, V, A, Negative](desc)

  /** Predicate that checks if a numeric value is zero or positive (>= 0) */
  def nonNegative[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonNegative]
  ): ConfigDescriptor[K, V, Refined[A, NonNegative]] =
    asRefined[K, V, A, NonNegative](desc)

  /** Predicate that checks if an integral value is evenly divisible by `N` */
  def divisible[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Divisible[N]]
  ): ConfigDescriptor[K, V, Refined[A, Divisible[N]]] =
    asRefined[K, V, A, Divisible[N]](desc)

  /** Predicate that checks if an integral value is not evenly divisible by `N` */
  def nonDivisible[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonDivisible[N]]
  ): ConfigDescriptor[K, V, Refined[A, NonDivisible[N]]] =
    asRefined[K, V, A, NonDivisible[N]](desc)

  /** Predicate that checks if an integral value is evenly divisible by 2 */
  def even[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Even]
  ): ConfigDescriptor[K, V, Refined[A, Even]] =
    asRefined[K, V, A, Even](desc)

  /** Predicate that checks if an integral value is not evenly divisible by 2 */
  def odd[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Odd]
  ): ConfigDescriptor[K, V, Refined[A, Odd]] =
    asRefined[K, V, A, Odd](desc)

}
