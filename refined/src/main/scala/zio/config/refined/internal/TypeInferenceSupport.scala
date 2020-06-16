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
  type K
  type V

  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Less[N]]
  ): ConfigDescriptor[Refined[A, Less[N]]] =
    asRefined[A, Less[N]](desc)
}

final class GreaterPartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Greater[N]]
  ): ConfigDescriptor[Refined[A, Greater[N]]] =
    asRefined[A, Greater[N]](desc)
}

final class LessEqualPartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, LessEqual[N]]
  ): ConfigDescriptor[Refined[A, LessEqual[N]]] =
    asRefined[A, LessEqual[N]](desc)
}

final class GreaterEqualPartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, GreaterEqual[N]]
  ): ConfigDescriptor[Refined[A, GreaterEqual[N]]] =
    asRefined[A, GreaterEqual[N]](desc)
}

final class DivisiblePartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Divisible[N]]
  ): ConfigDescriptor[Refined[A, Divisible[N]]] =
    asRefined[A, Divisible[N]](desc)
}

final class NonDivisiblePartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, NonDivisible[N]]
  ): ConfigDescriptor[Refined[A, NonDivisible[N]]] =
    asRefined[A, NonDivisible[N]](desc)
}

//// Collection

final class CountPartiallyApplied[PA, PC] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Count[PA, PC]]
  ): ConfigDescriptor[Refined[A, Count[PA, PC]]] =
    asRefined[A, Count[PA, PC]](desc)
}

final class EmptyPartiallyApplied {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Empty]
  ): ConfigDescriptor[Refined[A, Empty]] =
    asRefined[A, Empty](desc)
}

final class ForallPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Forall[P]]
  ): ConfigDescriptor[Refined[A, Forall[P]]] =
    asRefined[A, Forall[P]](desc)
}

final class HeadPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Head[P]]
  ): ConfigDescriptor[Refined[A, Head[P]]] =
    asRefined[A, Head[P]](desc)
}

final class IndexPartiallyApplied[N, P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Index[N, P]]
  ): ConfigDescriptor[Refined[A, Index[N, P]]] =
    asRefined[A, Index[N, P]](desc)
}

final class InitPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Init[P]]
  ): ConfigDescriptor[Refined[A, Init[P]]] =
    asRefined[A, Init[P]](desc)
}

final class LastPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Last[P]]
  ): ConfigDescriptor[Refined[A, Last[P]]] =
    asRefined[A, Last[P]](desc)
}

final class SizePartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Size[P]]
  ): ConfigDescriptor[Refined[A, Size[P]]] =
    asRefined[A, Size[P]](desc)
}

final class TailPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Tail[P]]
  ): ConfigDescriptor[Refined[A, Tail[P]]] =
    asRefined[A, Tail[P]](desc)
}

final class ContainsPartiallyApplied[U] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Contains[U]]
  ): ConfigDescriptor[Refined[A, Contains[U]]] =
    asRefined[A, Contains[U]](desc)
}

final class ExistsPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Exists[P]]
  ): ConfigDescriptor[Refined[A, Exists[P]]] =
    asRefined[A, Exists[P]](desc)
}

final class MinSizePartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, MinSize[N]]
  ): ConfigDescriptor[Refined[A, MinSize[N]]] =
    asRefined[A, MinSize[N]](desc)
}

final class MaxSizePartiallyApplied[N] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, MaxSize[N]]
  ): ConfigDescriptor[Refined[A, MaxSize[N]]] =
    asRefined[A, MaxSize[N]](desc)
}

//// String

final class EndsWithPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, EndsWith[S]]
  ): ConfigDescriptor[Refined[A, EndsWith[S]]] =
    asRefined[A, EndsWith[S]](desc)
}

final class MatchesRegexPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, MatchesRegex[S]]
  ): ConfigDescriptor[Refined[A, MatchesRegex[S]]] =
    asRefined[A, MatchesRegex[S]](desc)
}

final class StartsWithPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, StartsWith[S]]
  ): ConfigDescriptor[Refined[A, StartsWith[S]]] =
    asRefined[A, StartsWith[S]](desc)
}

//// Boolean

final class NotPartiallyApplied[P] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Not[P]]
  ): ConfigDescriptor[Refined[A, Not[P]]] =
    asRefined[A, Not[P]](desc)
}

final class AndPartiallyApplied[PA, PB] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, And[PA, PB]]
  ): ConfigDescriptor[Refined[A, And[PA, PB]]] =
    asRefined[A, And[PA, PB]](desc)
}

final class OrPartiallyApplied[PA, PB] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Or[PA, PB]]
  ): ConfigDescriptor[Refined[A, Or[PA, PB]]] =
    asRefined[A, Or[PA, PB]](desc)
}

final class XorPartiallyApplied[PA, PB] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Xor[PA, PB]]
  ): ConfigDescriptor[Refined[A, Xor[PA, PB]]] =
    asRefined[A, Xor[PA, PB]](desc)
}

final class AllOfPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, AllOf[S]]
  ): ConfigDescriptor[Refined[A, AllOf[S]]] =
    asRefined[A, AllOf[S]](desc)
}

final class AnyOfPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, AnyOf[S]]
  ): ConfigDescriptor[Refined[A, AnyOf[S]]] =
    asRefined[A, AnyOf[S]](desc)
}

final class OneOfPartiallyApplied[S] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, OneOf[S]]
  ): ConfigDescriptor[Refined[A, OneOf[S]]] =
    asRefined[A, OneOf[S]](desc)
}

final class NandPartiallyApplied[PA, PB] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Nand[PA, PB]]
  ): ConfigDescriptor[Refined[A, Nand[PA, PB]]] =
    asRefined[A, Nand[PA, PB]](desc)
}

final class NorPartiallyApplied[PA, PB] {
  def apply[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Nor[PA, PB]]
  ): ConfigDescriptor[Refined[A, Nor[PA, PB]]] =
    asRefined[A, Nor[PA, PB]](desc)
}
