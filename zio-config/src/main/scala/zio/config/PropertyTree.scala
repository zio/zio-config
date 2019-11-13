package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }

sealed trait PropertyTree[+K, +V] { self =>
  def flatten[K1 >: K, V1 >: V](zero: K1, append: (K1, K1) => K1): Map[K1, V1] = {
    def go(key: K1, propertyTree: PropertyTree[K1, V1], acc: Map[K1, V1]): Map[K1, V1] =
      propertyTree match {
        case Empty         => acc
        case Leaf(v)       => acc ++ Map(key -> v)
        case Record(value) => value.flatMap(t => go(append(key, t._1), t._2, acc))
      }

    go(zero, self, Map.empty[K1, V1])
  }

  def flattenString[K1 >: K, V1 >: V](
    appendString: String = "."
  )(implicit SK: String =:= K1, KS: K1 =:= String): Map[K1, V1] =
    self.flatten[K1, V1](
      SK(""),
      (a: K1, b: K1) => if (KS(a).nonEmpty) SK(List(SK(a), SK(b)).mkString(appendString)) else b
    )
}

object PropertyTree {

  final case object Empty extends PropertyTree[Nothing, Nothing]

  final case class Leaf[K, V](value: V) extends PropertyTree[Nothing, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf(t._2)))
}
