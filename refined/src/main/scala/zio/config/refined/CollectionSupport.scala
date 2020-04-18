package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.collection._
import zio.config.refined.internal._
import zio.config.string._

private[refined] trait CollectionSupport extends RefinedModule {

  /**
   * Predicate that counts the number of elements in a `Traversable`
   * which satisfy the predicate `PA` and passes the result to the numeric
   * predicate `PC`
   */
  def count[PA, PC]: CountPartiallyApplied[PA, PC] =
    new CountPartiallyApplied[PA, PC]

  /** Predicate that checks if a `Traversable` is empty */
  def empty[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, Empty]
  ): ConfigDescriptor[Refined[A, Empty]] =
    asRefined[A, Empty](desc)

  /**
   * Predicate that checks if the predicate `P` holds for all elements of a
   * `Traversable`
   */
  def forall[P]: ForallPartiallyApplied[P] =
    new ForallPartiallyApplied[P]

  /**
   * Predicate that checks if the predicate `P` holds for the first element
   * of a `Traversable`
   */
  def head[P]: HeadPartiallyApplied[P] =
    new HeadPartiallyApplied[P]

  /**
   * Predicate that checks if the predicate `P` holds for the element at
   * index `N` of a sequence
   */
  def index[N, P]: IndexPartiallyApplied[N, P] =
    new IndexPartiallyApplied[N, P]

  /**
   * Predicate that checks if the predicate `P` holds for all but the last
   * element of a `Traversable`
   */
  def init[P]: InitPartiallyApplied[P] =
    new InitPartiallyApplied[P]

  /**
   * Predicate that checks if the predicate `P` holds for the last element
   * of a `Traversable`
   */
  def last[P]: LastPartiallyApplied[P] =
    new LastPartiallyApplied[P]

  /**
   * Predicate that checks if the size of a `Traversable` satisfies the
   * predicate `P`
   */
  def size[P]: SizePartiallyApplied[P] =
    new SizePartiallyApplied[P]

  /**
   * Predicate that checks if the predicate `P` holds for all but the first
   * element of a `Traversable`
   */
  def tail[P]: TailPartiallyApplied[P] =
    new TailPartiallyApplied[P]

  /**
   * Predicate that checks if a `Traversable` contains a value
   * equal to `U`
   */
  def contains[U]: ContainsPartiallyApplied[U] =
    new ContainsPartiallyApplied[U]

  /**
   * Predicate that checks if the predicate `P` holds for some elements of a
   * `Traversable`
   */
  def exists[P]: ExistsPartiallyApplied[P] =
    new ExistsPartiallyApplied[P]

  /**
   * Predicate that checks if the size of a `Traversable` is greater than
   * or equal to `N`
   */
  def minSize[N]: MinSizePartiallyApplied[N] =
    new MinSizePartiallyApplied[N]

  /**
   * Predicate that checks if the size of a `Traversable` is less than
   * or equal to `N`
   */
  def maxSize[N]: MaxSizePartiallyApplied[N] =
    new MaxSizePartiallyApplied[N]

  /** Predicate that checks if a `Traversable` is not empty */
  def nonEmpty[A](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, NonEmpty]
  ): ConfigDescriptor[Refined[A, NonEmpty]] =
    asRefined[A, NonEmpty](desc)

}
