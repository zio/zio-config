package zio.config.refined
package internal

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.numeric.{ Divisible, Greater, GreaterEqual, Less, LessEqual, NonDivisible }
import zio.config.ConfigDescriptor

final class LessPartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Less[N]]
  ): ConfigDescriptor[K, V, Refined[A, Less[N]]] =
    asRefined[K, V, A, Less[N]](desc)
}

final class GreaterPartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Greater[N]]
  ): ConfigDescriptor[K, V, Refined[A, Greater[N]]] =
    asRefined[K, V, A, Greater[N]](desc)
}

final class LessEqualPartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, LessEqual[N]]
  ): ConfigDescriptor[K, V, Refined[A, LessEqual[N]]] =
    asRefined[K, V, A, LessEqual[N]](desc)
}

final class GreaterEqualPartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, GreaterEqual[N]]
  ): ConfigDescriptor[K, V, Refined[A, GreaterEqual[N]]] =
    asRefined[K, V, A, GreaterEqual[N]](desc)
}

final class DivisiblePartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Divisible[N]]
  ): ConfigDescriptor[K, V, Refined[A, Divisible[N]]] =
    asRefined[K, V, A, Divisible[N]](desc)
}

final class NonDivisiblePartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonDivisible[N]]
  ): ConfigDescriptor[K, V, Refined[A, NonDivisible[N]]] =
    asRefined[K, V, A, NonDivisible[N]](desc)
}
