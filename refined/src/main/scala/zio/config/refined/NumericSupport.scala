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
  def nonNaN[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, NonNaN]
  ): ConfigDescriptor[Refined[A, NonNaN]] =
    asRefined[A, NonNaN](desc)

  /** Predicate that checks if a numeric value is less than or equal to `N` */
  def lessEqual[N]: LessEqualPartiallyApplied[N] =
    new LessEqualPartiallyApplied[N]

  /** Predicate that checks if a numeric value is greater than or equal to `N` */
  def greaterEqual[N]: GreaterEqualPartiallyApplied[N] =
    new GreaterEqualPartiallyApplied[N]

  /** Predicate that checks if a numeric value is positive (> 0) */
  def positive[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Positive]
  ): ConfigDescriptor[Refined[A, Positive]] =
    asRefined[A, Positive](desc)

  /** Predicate that checks if a numeric value is zero or negative (<= 0) */
  def nonPositive[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, NonPositive]
  ): ConfigDescriptor[Refined[A, NonPositive]] =
    asRefined[A, NonPositive](desc)

  /** Predicate that checks if a numeric value is negative (< 0) */
  def negative[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Negative]
  ): ConfigDescriptor[Refined[A, Negative]] =
    asRefined[A, Negative](desc)

  /** Predicate that checks if a numeric value is zero or positive (>= 0) */
  def nonNegative[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, NonNegative]
  ): ConfigDescriptor[Refined[A, NonNegative]] =
    asRefined[A, NonNegative](desc)

  /** Predicate that checks if an integral value is evenly divisible by `N` */
  def divisible[N]: DivisiblePartiallyApplied[N] =
    new DivisiblePartiallyApplied[N]

  /** Predicate that checks if an integral value is not evenly divisible by `N` */
  def nonDivisible[N]: NonDivisiblePartiallyApplied[N] =
    new NonDivisiblePartiallyApplied[N]

  /** Predicate that checks if an integral value is evenly divisible by 2 */
  def even[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Even]
  ): ConfigDescriptor[Refined[A, Even]] =
    asRefined[A, Even](desc)

  /** Predicate that checks if an integral value is not evenly divisible by 2 */
  def odd[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Odd]
  ): ConfigDescriptor[Refined[A, Odd]] =
    asRefined[A, Odd](desc)

}
