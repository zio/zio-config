package zio.config

sealed trait PropertyTree[+K, +V]

object PropertyTree {
  final case class Leaf[V](value: V)                               extends PropertyTree[Nothing, V]
  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf(t._2)))
}
