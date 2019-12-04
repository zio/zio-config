package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.numeric._
import zio.config.ConfigDescriptor
import zio.config.refined.internal._

private[refined] trait NumericSupport {

  /** Predicate that checks if a numeric value is less than `N` */
  def less[N]: LessPartiallyApplied[N] =
    new LessPartiallyApplied[N]

  /** Predicate that checks if a numeric value is greater than `N` */
  def greater[N]: GreaterPartiallyApplied[N] =
    new GreaterPartiallyApplied[N]

  /** Predicate that checks if a floating-point number value is not NaN */
  def nonNaN[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonNaN]
  ): ConfigDescriptor[K, V, Refined[A, NonNaN]] =
    asRefined[K, V, A, NonNaN](desc)

  /** Predicate that checks if a numeric value is less than or equal to `N` */
  def lessEqual[N]: LessEqualPartiallyApplied[N] =
    new LessEqualPartiallyApplied[N]

  /** Predicate that checks if a numeric value is greater than or equal to `N` */
  def greaterEqual[N]: GreaterEqualPartiallyApplied[N] =
    new GreaterEqualPartiallyApplied[N]

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
  def divisible[N]: DivisiblePartiallyApplied[N] =
    new DivisiblePartiallyApplied[N]

  /** Predicate that checks if an integral value is not evenly divisible by `N` */
  def nonDivisible[N]: NonDivisiblePartiallyApplied[N] =
    new NonDivisiblePartiallyApplied[N]

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
