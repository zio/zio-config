package zio.config

import _root_.scalaz.Isomorphism.<=>
import _root_.scalaz.{==>>, IList, ISet, Maybe, Order}
import zio.config._

import Config._

package object scalaz {
  def fromIso[A, B](D: A <=> B)(M: Config[A]): Config[B] =
    M.transform[B](D.to, D.from)

  def iList[A](config: Config[A]): Config[IList[A]] =
    listOf(config).transform(IList.fromList, _.toList)

  def iSet[A: Order](config: Config[A]): Config[ISet[A]] =
    listOf(config).transform(l => ISet.fromList(l), _.toList)

  def mapz[A: Order, B](config: Config[Map[A, B]]): Config[A ==>> B] =
    config.transform(m => ==>>.fromList(m.toList), _.toList.toMap)

  def maybe[A](config: Config[A]): Config[Maybe[A]] =
    config.optional.transform(Maybe.fromOption, _.toOption)
}
