package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.boolean._
import zio.config.refined.internal._
import zio.config.string._

private[refined] trait BooleanSupport extends RefinedModule {

  /** Constant predicate that is always `true` */
  def pTrue[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, True]
  ): ConfigDescriptor[Refined[A, True]] =
    asRefined[A, True](desc)

  /** Constant predicate that is always `false` */
  def pFalse[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, False]
  ): ConfigDescriptor[Refined[A, False]] =
    asRefined[A, False](desc)

  /** Negation of the predicate `P` */
  def not[P]: NotPartiallyApplied[P] =
    new NotPartiallyApplied[P]

  /** Conjunction of the predicates `A` and `B` */
  def and[A, B]: AndPartiallyApplied[A, B] =
    new AndPartiallyApplied[A, B]

  /** Disjunction of the predicates `A` and `B` */
  def or[A, B]: OrPartiallyApplied[A, B] =
    new OrPartiallyApplied[A, B]

  /** Exclusive disjunction of the predicates `A` and `B` */
  def xor[A, B]: XorPartiallyApplied[A, B] =
    new XorPartiallyApplied[A, B]

  /** Conjunction of all predicates in `PS` */
  def allOf[S]: AllOfPartiallyApplied[S] =
    new AllOfPartiallyApplied[S]

  /** Disjunction of all predicates in `PS` */
  def anyOf[S]: AnyOfPartiallyApplied[S] =
    new AnyOfPartiallyApplied[S]

  /** Exclusive disjunction of all predicates in `PS` */
  def oneOf[S]: OneOfPartiallyApplied[S] =
    new OneOfPartiallyApplied[S]

  /** Negated conjunction of the predicates `A` and `B` */
  def nand[A, B]: NandPartiallyApplied[A, B] =
    new NandPartiallyApplied[A, B]

  /** Negated disjunction of the predicates `A` and `B` */
  def nor[A, B]: NorPartiallyApplied[A, B] =
    new NorPartiallyApplied[A, B]

}
