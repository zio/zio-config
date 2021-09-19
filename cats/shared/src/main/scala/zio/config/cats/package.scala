package zio.config

import scala.collection.immutable.SortedMap
import _root_.cats.data._
import _root_.cats.implicits._
import _root_.cats.kernel.Order

import zio.config._, ConfigDescriptor._

package object cats {
  def chain[A](aDesc: ConfigDescriptor[A]): ConfigDescriptor[Chain[A]] =
    list(aDesc).transform(v => Chain.fromSeq(v), _.toList)

  def chain[A](path: String)(aDesc: ConfigDescriptor[A]): ConfigDescriptor[Chain[A]] =
    nested(path)(chain(aDesc))

  def nonEmptyChain[A](aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyChain[A]] =
    chain(aDesc).transformOrFailLeft(value =>
      NonEmptyChain.fromChain(value).fold[Either[String, NonEmptyChain[A]]](Left("chain is empty"))(value => Right(value))
    )(
      _.toChain
    )

  def nonEmptyChain[A](path: String)(aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyChain[A]] =
    nested(path)(nonEmptyChain(aDesc))

  def nonEmptyList[A](aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyList[A]] =
    list(aDesc).transformOrFailLeft(value =>
      NonEmptyList.fromList(value).fold[Either[String, NonEmptyList[A]]](Left("list is empty"))(v => Right(v))
    )(
      _.toList
    )

  def nonEmptyList[A](path: String)(aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyList[A]] =
    nested(path)(nonEmptyList(aDesc))

  def nonEmptyMap[A](aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyMap[String, A]] =
    map(aDesc).transformOrFailLeft(x =>
      NonEmptyMap
        .fromMap(SortedMap(x.toSeq: _*)(Order[String].toOrdering))
        .fold[Either[String, NonEmptyMap[String, A]]](Left("map is empty"))(v => Right(v))
    )(
      _.toSortedMap
    )

  def nonEmptyMap[A](path: String)(aDesc: ConfigDescriptor[A]): ConfigDescriptor[NonEmptyMap[String, A]] =
    nested(path)(nonEmptyMap(aDesc))
}
