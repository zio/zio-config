package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }

sealed trait PropertyTree[K, V] { self =>
  def flatten(zero: K, append: (K, K) => K): Map[K, V] = {
    def go(key: K, propertyTree: PropertyTree[K, V], acc: Map[K, V]): Map[K, V] =
      propertyTree match {
        case Empty()         => acc
        case Leaf(v)       => acc.updated(key, v)
        case Record(value) => value.flatMap(t => go(append(key, t._1), t._2, acc))
      }

    go(zero, self, Map.empty[K, V])
  }

  def flattenString(
    appendString: String = "."
  )(implicit SK: String =:= K, KS: K =:= String): Map[K, V] =
    self.flatten(
      SK(""),
      (a: K, b: K) => if (KS(a).nonEmpty) SK(List(SK(a), SK(b)).mkString(appendString)) else b
    )
}

object PropertyTree {

  final case class Empty[K, V]() extends PropertyTree[K, V]

  final case class Leaf[K, V](value: V) extends PropertyTree[K, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[K, V](t._2)))
}
