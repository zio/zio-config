package zio.config

import _root_.scalaz.Isomorphism.<=>
import _root_.scalaz.{==>>, IList, ISet, Maybe, Order}
import zio.config._

import ConfigDescriptor._

package object scalaz {
  def fromIso[A, B](D: A <=> B)(M: ConfigDescriptor[A]): ConfigDescriptor[B] =
    M.transform[B](D.to, D.from)

  def iList[A](config: ConfigDescriptor[A]): ConfigDescriptor[IList[A]] =
    list(config).transform(IList.fromList, _.toList)

  def iSet[A: Order](config: ConfigDescriptor[A]): ConfigDescriptor[ISet[A]] =
    list(config).transform(l => ISet.fromList(l), _.toList)

  def mapz[A: Order, B](config: ConfigDescriptor[Map[A, B]]): ConfigDescriptor[A ==>> B] =
    config.transform(m => ==>>.fromList(m.toList), _.toList.toMap)

  def maybe[A](config: ConfigDescriptor[A]): ConfigDescriptor[Maybe[A]] =
    config.optional.transform(Maybe.fromOption, _.toOption)
}
