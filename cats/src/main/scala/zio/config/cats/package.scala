package zio.config

import _root_.cats.data._
import _root_.cats.implicits._
import _root_.cats.kernel.Order
import zio.Config

import scala.collection.immutable.SortedMap

import Config._

package object cats {
  def chain[A](aDesc: Config[A]): Config[Chain[A]] =
    listOf(aDesc).map(v => Chain.fromSeq(v))

  def chain[A](path: String)(aDesc: Config[A]): Config[Chain[A]] =
    chain(aDesc).nested(path)

  def nonEmptyChain[A](aDesc: Config[A]): Config[NonEmptyChain[A]] =
    chain(aDesc).mapOrFail(value =>
      NonEmptyChain
        .fromChain(value)
        .fold[Either[Config.Error, NonEmptyChain[A]]](Left(Config.Error.InvalidData(message = "chain is empty")))(
          value => Right(value)
        )
    )

  def nonEmptyChain[A](path: String)(aDesc: Config[A]): Config[NonEmptyChain[A]] =
    nonEmptyChain(aDesc).nested(path)

  def nonEmptyList[A](aDesc: Config[A]): Config[NonEmptyList[A]] =
    listOf(aDesc).mapOrFail(value =>
      NonEmptyList
        .fromList(value)
        .fold[Either[Config.Error, NonEmptyList[A]]](Left(Config.Error.InvalidData(message = "list is empty")))(v =>
          Right(v)
        )
    )

  def nonEmptyList[A](path: String)(aDesc: Config[A]): Config[NonEmptyList[A]] =
    nonEmptyList(aDesc).nested(path)

  def nonEmptyMap[A](aDesc: Config[A]): Config[NonEmptyMap[String, A]] =
    table(aDesc).mapOrFail(x =>
      NonEmptyMap
        .fromMap(SortedMap(x.toSeq: _*)(Order[String].toOrdering))
        .fold[Either[Config.Error, NonEmptyMap[String, A]]](Left(Config.Error.InvalidData(message = "map is empty")))(
          v => Right(v)
        )
    )

  def nonEmptyMap[A](path: String)(aDesc: Config[A]): Config[NonEmptyMap[String, A]] =
    nonEmptyMap(aDesc).nested(path)
}
