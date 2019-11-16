package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }

sealed trait PropertyTree[K, V] { self =>
  def flatten: Map[Vector[K], V] = {
    def go(key: Vector[K], propertyTree: PropertyTree[K, V], acc: Map[Vector[K], V]): Map[Vector[K], V] =
      propertyTree match {
        case Empty()       => acc
        case Leaf(v)       => acc.updated(key, v)
        case Record(value) => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K], V])
  }

  def get(key: K): Option[V] = {
    def loop(proper: PropertyTree[K, V]): Option[V] =
      proper match {
        case Empty()       => None
        case Leaf(v)       => Some(v)
        case Record(value) => value.get(key).flatMap(loop)
      }
    loop(self)
  }

  def flattenString(
    appendString: String = "."
  )(implicit SK: String =:= K, KS: K =:= String): Map[K, V] =
    self.flatten.map({ case (key, value) => (key.map(KS).mkString(appendString), value) })
}

object PropertyTree {

  final case class Empty[K, V]() extends PropertyTree[K, V]

  final case class Leaf[K, V](value: V) extends PropertyTree[K, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[K, V](t._2)))
}
