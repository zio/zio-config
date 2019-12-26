package zio.config

import zio.config.PropertyTree.{ Empty, Leaf, Record }
import zio.config.PropertyTree.Sequence

sealed trait PropertyTree[+K, +V] { self =>
  final def flatten[K1 >: K, V1 >: V]: Map[Vector[K1], ::[V1]] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], ::[V1]]): Map[Vector[K1], ::[V1]] =
      propertyTree match {
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v)         => acc.updated(key, ::(v, Nil))
        case Record(value)   => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], ::[V]])
  }

  final def get[K1 >: K](key: K1): List[V] = {
    def loop(proper: PropertyTree[K1, V]): List[V] =
      proper match {
        case Leaf(v)              => List(v)
        case Sequence(value)      => value.flatMap(loop)
        case record: Record[K, V] => record.value.get(key.asInstanceOf[K]).toList.flatMap(loop)
      }
    loop(self)
  }

  final def merge[K1 >: K, V1 >: V](that: PropertyTree[K1, V1]): PropertyTree[K1, V1] =
    (self, that) match {
      case (Sequence(l), Sequence(r)) => Sequence(l ++ r)
      case (l, Sequence(r))           => Sequence(List(l) ++ r)
      case (Sequence(l), r)           => Sequence(l ++ List(r))
      case (l: Record[K, V], r: Record[K1, V1]) =>
        Record[K1, V1](
          (l.value.toList ::: r.value.toList)
            .groupBy(_._1)
            .map {
              case (k, v) => (k, v.map(_._2).reduceOption((a, b) => a.merge(b)).getOrElse(Empty))
            }
            .toMap
        )
      case (l, r) => Sequence(l :: r :: Nil)
    }

  final def <>[K1 >: K, V1 >: V](that: PropertyTree[K1, V1]): PropertyTree[K1, V1] =
    self.merge(that)

  final def flattenString[K1 >: K, V1 >: V](
    appendString: String = "."
  )(implicit KS: K1 =:= String): Map[String, ::[V1]] = flattenWith(KS)(appendString)

  final def flattenWith[K1 >: K, V1 >: V](f: K1 => String)(
    appendString: String = "."
  ): Map[String, ::[V1]] =
    self.flatten[K1, V1].map({ case (key, value) => (key.map(f).mkString(appendString), value) })
}

object PropertyTree {

  final case class Leaf[K, V](value: V) extends PropertyTree[K, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  final case class Sequence[K, V](value: List[PropertyTree[K, V]]) extends PropertyTree[K, V]

  val Empty: PropertyTree[Nothing, Nothing] = Sequence(Nil)

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[K, V](t._2)))

}
