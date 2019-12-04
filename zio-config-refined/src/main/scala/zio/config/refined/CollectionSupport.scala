package zio.config.refined

import eu.timepit.refined.api.{ Refined, Validate }
import eu.timepit.refined.collection._
import zio.config.ConfigDescriptor

private[refined] trait CollectionSupport {

  /**
   * Predicate that counts the number of elements in a `Traversable`
   * which satisfy the predicate `PA` and passes the result to the numeric
   * predicate `PC`
   */
  def count[K, V, A, PA, PC](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Count[PA, PC]]
  ): ConfigDescriptor[K, V, Refined[A, Count[PA, PC]]] =
    asRefined[K, V, A, Count[PA, PC]](desc)

  /** Predicate that checks if a `Traversable` is empty. */
  def empty[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Empty]
  ): ConfigDescriptor[K, V, Refined[A, Empty]] =
    asRefined[K, V, A, Empty](desc)

  /**
   * Predicate that checks if the predicate `P` holds for all elements of a
   * `Traversable`
   */
  def forall[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Forall[P]]
  ): ConfigDescriptor[K, V, Refined[A, Forall[P]]] =
    asRefined[K, V, A, Forall[P]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for the first element
   * of a `Traversable`
   */
  def head[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Head[P]]
  ): ConfigDescriptor[K, V, Refined[A, Head[P]]] =
    asRefined[K, V, A, Head[P]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for the element at
   * index `N` of a sequence
   */
  def index[K, V, A, N, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Index[N, P]]
  ): ConfigDescriptor[K, V, Refined[A, Index[N, P]]] =
    asRefined[K, V, A, Index[N, P]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for all but the last
   * element of a `Traversable`
   */
  def init[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Init[P]]
  ): ConfigDescriptor[K, V, Refined[A, Init[P]]] =
    asRefined[K, V, A, Init[P]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for the last element
   * of a `Traversable`
   */
  def last[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Last[P]]
  ): ConfigDescriptor[K, V, Refined[A, Last[P]]] =
    asRefined[K, V, A, Last[P]](desc)

  /**
   * Predicate that checks if the size of a `Traversable` satisfies the
   * predicate `P`
   */
  def size[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Size[P]]
  ): ConfigDescriptor[K, V, Refined[A, Size[P]]] =
    asRefined[K, V, A, Size[P]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for all but the first
   * element of a `Traversable`
   */
  def tail[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Tail[P]]
  ): ConfigDescriptor[K, V, Refined[A, Tail[P]]] =
    asRefined[K, V, A, Tail[P]](desc)

  /**
   * Predicate that checks if a `Traversable` contains a value
   * equal to `U`
   */
  def contains[K, V, A, U](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Contains[U]]
  ): ConfigDescriptor[K, V, Refined[A, Contains[U]]] =
    asRefined[K, V, A, Contains[U]](desc)

  /**
   * Predicate that checks if the predicate `P` holds for some elements of a
   * `Traversable`
   */
  def exists[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, Exists[P]]
  ): ConfigDescriptor[K, V, Refined[A, Exists[P]]] =
    asRefined[K, V, A, Exists[P]](desc)

  /**
   * Predicate that checks if the size of a `Traversable` is greater than
   * or equal to `N`
   */
  def minSize[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, MinSize[N]]
  ): ConfigDescriptor[K, V, Refined[A, MinSize[N]]] =
    asRefined[K, V, A, MinSize[N]](desc)

  /**
   * Predicate that checks if the size of a `Traversable` is less than
   * or equal to `N`
   */
  def maxSize[K, V, A, N](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, MaxSize[N]]
  ): ConfigDescriptor[K, V, Refined[A, MaxSize[N]]] =
    asRefined[K, V, A, MaxSize[N]](desc)

  /** Predicate that checks if a `Traversable` is not empty. */
  def nonEmpty[K, V, A](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, NonEmpty]
  ): ConfigDescriptor[K, V, Refined[A, NonEmpty]] =
    asRefined[K, V, A, NonEmpty](desc)

}
