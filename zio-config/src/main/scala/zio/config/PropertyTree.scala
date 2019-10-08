package zio.config

sealed trait PropertyTree

object PropertyTree {
  final case class Leaf(value: String) extends PropertyTree
  final case class Record(value: Map[String, PropertyTree]) extends PropertyTree

  def fromMap(map: Map[String, String]): PropertyTree =
    Record(map.map(t => t._1 -> Leaf(t._2)))
}
