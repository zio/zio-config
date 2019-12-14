package zio.config.refined
package internal

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.boolean._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import zio.config.ConfigDescriptor

// This collection of classes is to ease type inference, as ordained by
// high priest Rob Norris http://tpolecat.github.io/2015/07/30/infer.html

//// Numeric

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

//// Collection

final class CountPartiallyApplied[PA, PC] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Count[PA, PC]]
  ): ConfigDescriptor[K, V, Refined[A, Count[PA, PC]]] =
    asRefined[K, V, A, Count[PA, PC]](desc)
}

final class EmptyPartiallyApplied {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Empty]
  ): ConfigDescriptor[K, V, Refined[A, Empty]] =
    asRefined[K, V, A, Empty](desc)
}

final class ForallPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Forall[P]]
  ): ConfigDescriptor[K, V, Refined[A, Forall[P]]] =
    asRefined[K, V, A, Forall[P]](desc)
}

final class HeadPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Head[P]]
  ): ConfigDescriptor[K, V, Refined[A, Head[P]]] =
    asRefined[K, V, A, Head[P]](desc)
}

final class IndexPartiallyApplied[N, P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Index[N, P]]
  ): ConfigDescriptor[K, V, Refined[A, Index[N, P]]] =
    asRefined[K, V, A, Index[N, P]](desc)
}

final class InitPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Init[P]]
  ): ConfigDescriptor[K, V, Refined[A, Init[P]]] =
    asRefined[K, V, A, Init[P]](desc)
}

final class LastPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Last[P]]
  ): ConfigDescriptor[K, V, Refined[A, Last[P]]] =
    asRefined[K, V, A, Last[P]](desc)
}

final class SizePartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Size[P]]
  ): ConfigDescriptor[K, V, Refined[A, Size[P]]] =
    asRefined[K, V, A, Size[P]](desc)
}

final class TailPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Tail[P]]
  ): ConfigDescriptor[K, V, Refined[A, Tail[P]]] =
    asRefined[K, V, A, Tail[P]](desc)
}

final class ContainsPartiallyApplied[U] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Contains[U]]
  ): ConfigDescriptor[K, V, Refined[A, Contains[U]]] =
    asRefined[K, V, A, Contains[U]](desc)
}

final class ExistsPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Exists[P]]
  ): ConfigDescriptor[K, V, Refined[A, Exists[P]]] =
    asRefined[K, V, A, Exists[P]](desc)
}

final class MinSizePartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, MinSize[N]]
  ): ConfigDescriptor[K, V, Refined[A, MinSize[N]]] =
    asRefined[K, V, A, MinSize[N]](desc)
}

final class MaxSizePartiallyApplied[N] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, MaxSize[N]]
  ): ConfigDescriptor[K, V, Refined[A, MaxSize[N]]] =
    asRefined[K, V, A, MaxSize[N]](desc)
}

//// String

final class EndsWithPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, EndsWith[S]]
  ): ConfigDescriptor[K, V, Refined[A, EndsWith[S]]] =
    asRefined[K, V, A, EndsWith[S]](desc)
}

final class MatchesRegexPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, MatchesRegex[S]]
  ): ConfigDescriptor[K, V, Refined[A, MatchesRegex[S]]] =
    asRefined[K, V, A, MatchesRegex[S]](desc)
}

final class StartsWithPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, StartsWith[S]]
  ): ConfigDescriptor[K, V, Refined[A, StartsWith[S]]] =
    asRefined[K, V, A, StartsWith[S]](desc)
}

//// Boolean

final class NotPartiallyApplied[P] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Not[P]]
  ): ConfigDescriptor[K, V, Refined[A, Not[P]]] =
    asRefined[K, V, A, Not[P]](desc)
}

final class AndPartiallyApplied[A, B] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, And[A, B]]
  ): ConfigDescriptor[K, V, Refined[A, And[A, B]]] =
    asRefined[K, V, A, And[A, B]](desc)
}

final class OrPartiallyApplied[A, B] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Or[A, B]]
  ): ConfigDescriptor[K, V, Refined[A, Or[A, B]]] =
    asRefined[K, V, A, Or[A, B]](desc)
}

final class XorPartiallyApplied[A, B] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Xor[A, B]]
  ): ConfigDescriptor[K, V, Refined[A, Xor[A, B]]] =
    asRefined[K, V, A, Xor[A, B]](desc)
}

final class AllOfPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, AllOf[S]]
  ): ConfigDescriptor[K, V, Refined[A, AllOf[S]]] =
    asRefined[K, V, A, AllOf[S]](desc)
}

final class AnyOfPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, AnyOf[S]]
  ): ConfigDescriptor[K, V, Refined[A, AnyOf[S]]] =
    asRefined[K, V, A, AnyOf[S]](desc)
}

final class OneOfPartiallyApplied[S] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, OneOf[S]]
  ): ConfigDescriptor[K, V, Refined[A, OneOf[S]]] =
    asRefined[K, V, A, OneOf[S]](desc)
}

final class NandPartiallyApplied[A, B] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Nand[A, B]]
  ): ConfigDescriptor[K, V, Refined[A, Nand[A, B]]] =
    asRefined[K, V, A, Nand[A, B]](desc)
}

final class NorPartiallyApplied[A, B] {
  def apply[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Nor[A, B]]
  ): ConfigDescriptor[K, V, Refined[A, Nor[A, B]]] =
    asRefined[K, V, A, Nor[A, B]](desc)
}
