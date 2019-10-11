package zio.config

sealed trait PropertyTree[+K, +V]

object PropertyTree {

  final case object Empty extends PropertyTree[Nothing, Nothing]

  final case class Leaf[V](value: V) extends PropertyTree[Nothing, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf(t._2)))

  def flattenKV[K, V](propertyTree: PropertyTree[K, V], zero: K, append: (K, K) => K): Map[K, V] = {
    def go(key: K, propertyTree: PropertyTree[K, V], acc: Map[K, V]): Map[K, V] =
      propertyTree match {
        case Empty         => acc
        case Leaf(v)       => acc ++ Map(key -> v)
        case Record(value) => value.flatMap(t => go(append(key, t._1), t._2, acc))
      }

    go(zero, propertyTree, Map.empty)
  }

  def flatten(propertyTree: PropertyTree[String, String], appendString: String = "."): Map[String, String] =
    PropertyTree.flattenKV(
      propertyTree,
      "",
      (a: String, b: String) => if (a.nonEmpty) List(a, b).mkString(appendString) else b
    )
}
