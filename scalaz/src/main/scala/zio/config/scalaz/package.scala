package zio.config

import _root_.scalaz.{==>>, IList, ISet, Maybe, Order}
import zio.Config

import Config._

package object scalaz {
  def iList[A](config: Config[A]): Config[IList[A]] =
    listOf(config).map(IList.fromList)

  def iSet[A: Order](config: Config[A]): Config[ISet[A]] =
    listOf(config).map(l => ISet.fromList(l))

  def mapz[A: Order, B](config: Config[Map[A, B]]): Config[A ==>> B] =
    config.map(m => ==>>.fromList(m.toList))

  def maybe[A](config: Config[A]): Config[Maybe[A]] =
    config.optional.map(Maybe.fromOption)
}
