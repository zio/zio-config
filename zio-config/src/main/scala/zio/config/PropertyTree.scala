package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }

sealed trait PropertyTree[+K, +V] { self =>
  def flatten[K1 >: K]: Map[Vector[K1], V] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], V]): Map[Vector[K1], V] =
      propertyTree match {
        case Empty()       => acc
        case Leaf(v)       => acc.updated(key, v)
        case Record(value) => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], V])
  }

  def get[K1 >: K](key: K1): Option[V] = {
    def loop(proper: PropertyTree[K1, V]): Option[V] =
      proper match {
        case Empty()       => None
        case Leaf(v)       => Some(v)
        case Record(value) =>
          // To get over the GADT skolem with .get
          value.find(_._1 == key).map(_._2).flatMap(loop)
      }
    loop(self)
  }

  def flattenString[K1 >: K](
    appendString: String = "."
  )(implicit KS: K1 =:= String): Map[String, V] =
    self.flatten[K1].map({ case (key, value) => (key.map(KS).mkString(appendString), value) })
}

object PropertyTree {

  final case class Empty[K, V]() extends PropertyTree[K, V]

  final case class Leaf[K, V](value: V) extends PropertyTree[K, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[K, V](t._2)))
}
