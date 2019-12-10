package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }

sealed trait PropertyTree[+K, +V] { self =>
  final def flatten[K1 >: K]: Map[Vector[K1], V] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], V]): Map[Vector[K1], V] =
      propertyTree match {
        case Empty         => acc
        case Leaf(v)       => acc.updated(key, v)
        case Record(value) => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], V])
  }

  final def get[K1 >: K](key: K1): Option[V] = {
    def loop(proper: PropertyTree[K1, V]): Option[V] =
      proper match {
        case Empty                => None
        case Leaf(v)              => Some(v)
        case record: Record[K, V] => record.value.get(key.asInstanceOf[K]).flatMap(loop)
      }
    loop(self)
  }

  def add[K1 >: K, V1 >: V](v2: PropertyTree[K1, V1]): PropertyTree[K1, V1] =
    self match {
      case Empty => v2
      case Leaf(value) =>
        v2 match {
          case Empty         => Leaf(value.asInstanceOf[V1])
          case Leaf(value)   => Leaf(value)
          case Record(value) => Record(value)
        }
      case record: Record[K, V] =>
        v2 match {
          case Empty            => record
          case Leaf(_)          => record
          case rc: Record[K, V] => Record(PropertyTree.addPropertyMaps(rc.value, record.value))
        }
    }

  final def flattenString[K1 >: K](
    appendString: String = "."
  )(implicit KS: K1 =:= String): Map[String, V] = flattenWith(KS)(appendString)

  final def flattenWith[K1 >: K](f: K1 => String)(
    appendString: String = "."
  ): Map[String, V] =
    self.flatten[K1].map({ case (key, value) => (key.map(f).mkString(appendString), value) })
}

object PropertyTree {

  final case object Empty extends PropertyTree[Nothing, Nothing]

  final case class Leaf[K, V](value: V) extends PropertyTree[K, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[K, V](t._2)))

  private def addPropertyMaps[K, V](
    x: Map[K, PropertyTree[K, V]],
    y: Map[K, PropertyTree[K, V]]
  ): Map[K, PropertyTree[K, V]] =
    (x.toList ::: y.toList)
      .groupBy(_._1)
      .map {
        case (k, v) => (k, v.map(_._2).reduceOption((a, b) => a.add(b)).getOrElse(Empty))
      }
      .toMap
}
