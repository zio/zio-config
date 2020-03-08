package zio.config

import scala.collection.immutable.Nil
import PropertyTree._

sealed trait PropertyTree[+K, +V] { self =>
  final def map[V2](f: V => V2): PropertyTree[K, V2] = self match {
    case Leaf(value)     => Leaf(f(value))
    case Record(v)       => Record(v.map { case (k, tree) => (k, tree.map(f)) })
    case Sequence(value) => Sequence(value.map(_.map(f)))
  }

  final def map2[K1 >: K, V2, V3](that: PropertyTree[K1, V2])(f: (V, V2) => V3): Option[PropertyTree[K, V3]] =
    (self, that) match {
      case (left, _) if left.isEmpty   => None
      case (_, right) if right.isEmpty => None
      case (Sequence(l), Sequence(r))  =>
        // Zip ?
        val result = l.zip(r).map(tuple => tuple._1.map2(tuple._2)(f))
        seqOption(result).map(Sequence(_))

      case (Record(l), Record(r)) =>
        val result: Map[K, Option[PropertyTree[K, V3]]] =
          l.zip(r)
            .toList
            .map {
              case ((k, left), (_, right)) => k -> left.map2(right)(f)
            }
            .toMap

        seqMap(result).map(Record(_))

      case (Leaf(l), Leaf(r)) => Some(Leaf(f(l, r)))
      case (Sequence(l), Leaf(r)) =>
        if (l.size != 1) None else l.head.map2(Leaf(r))(f)
      case (Leaf(l), Sequence(r)) =>
        if (r.size != 1) None else Leaf(l).map2(r.head)(f)
    }

  final def zip[K1 >: K, V2, V3](that: PropertyTree[K1, V2]): Option[PropertyTree[K, (V, V2)]] =
    self.map2(that)((a, b) => ((a, b)))

  final def flatten[K1 >: K, V1 >: V]: Map[Vector[K1], ::[V1]] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], ::[V1]]): Map[Vector[K1], ::[V1]] =
      propertyTree match {
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v)         => acc.updated(key, ::(v, Nil))
        case Record(value)   => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], ::[V1]])
  }

  def isEmpty: Boolean = self match {
    case Leaf(_)         => false
    case Record(value)   => value.values.forall(_.isEmpty)
    case Sequence(value) => value.forall(_.isEmpty)
  }

  final def getPrefix[K1 >: K](key: K1): List[V] = {
    def loop(proper: PropertyTree[K1, V]): List[V] =
      proper match {
        case Leaf(v)              => List(v)
        case Sequence(value)      => value.flatMap(loop)
        case record: Record[K, V] => record.value.get(key.asInstanceOf[K]).toList.flatMap(loop)
      }
    loop(self)
  }

  def getPath[K1 >: K](k: List[K1]): Option[PropertyTree[K1, V]] =
    k match {
      case Nil => Some(self)
      case head :: next =>
        self match {
          case Leaf(_)       => None
          case Record(value) => value.get(head.asInstanceOf[K]).flatMap(_.getPath(next))
          case Sequence(_)   => None
        }
    }

  final def merge[K1 >: K, V1 >: V](that: PropertyTree[K1, V1]): List[PropertyTree[K1, V1]] =
    (self, that) match {
      case (left, right) if left.isEmpty  => singleton(right)
      case (left, right) if right.isEmpty => singleton(left)
      case (Sequence(l), Sequence(r))     => singleton(Sequence(l ++ r))
      case (l: Record[K, V], r: Record[K1, V1]) =>
        (l.value.keySet ++ r.value.keySet)
          .foldLeft(List[Map[K1, PropertyTree[K1, V1]]](Map.empty)) {
            case (acc, k) =>
              (l.value.get(k.asInstanceOf[K]), r.value.get(k)) match {
                case (None, None) => acc
                case (Some(l), Some(r)) =>
                  l.merge(r).map(tree => (k, tree)).flatMap(tuple => acc.map(map => map + tuple))
                case (Some(l), None) => acc.map(map => map + (k -> l))
                case (None, Some(r)) => acc.map(map => map + (k -> r))
              }
          }
          .map(v => PropertyTree.Record(v))
      case (l, r) => ::(l, r :: Nil)
    }

  final def flattenString[K1 >: K, V1 >: V](
    appendString: String = "."
  )(implicit KS: K1 =:= String): Map[String, ::[V1]] = flattenWith[K1, V1](KS)(appendString)

  final def flattenWith[K1 >: K, V1 >: V](f: K1 => String)(
    appendString: String = "."
  ): Map[String, ::[V1]] =
    self.flatten[K1, V1].map({ case (key, value) => (key.map(f).mkString(appendString), value) })
}

object PropertyTree {

  final case class Leaf[V](value: V) extends PropertyTree[Nothing, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  final case class Sequence[K, V](value: List[PropertyTree[K, V]]) extends PropertyTree[K, V]

  val empty: PropertyTree[Nothing, Nothing] = Sequence(Nil)

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[V](t._2)))

  def fromStringMap(map: Map[String, String], keySep: Char, valueSep: Char): List[PropertyTree[String, String]] =
    unflatten(
      map.map(
        tuple =>
          tuple._1.split(keySep).toVector.filterNot(_.trim == "") ->
            (tuple._2
              .split(valueSep)
              .toList match {
              case h :: tail =>
                ::(h, tail)
              case Nil => singleton(tuple._2)
            })
      )
    )

  def unflatten[K, V](key: List[K], value: ::[V]): PropertyTree[K, V] =
    key match {
      case head :: next => Record(Map(head -> unflatten(next, value)))
      case Nil          => Sequence(value.map(Leaf(_)))
    }

  def unflatten[K, V](map: Map[Vector[K], ::[V]]): List[PropertyTree[K, V]] =
    mergeAll(map.toList.map(tuple => unflatten(tuple._1.toList, tuple._2)))

  def mergeAll[K, V](list: List[PropertyTree[K, V]]): List[PropertyTree[K, V]] =
    list.foldLeft(List[PropertyTree[K, V]](PropertyTree.empty)) {
      case (acc, tree) => acc.flatMap(tree0 => tree.merge(tree0))
    }

  def sequence[K, V](tree: PropertyTree[K, V]): Option[PropertyTree[K, List[V]]] = {
    def convertList(f: List[PropertyTree[K, V]]): Option[PropertyTree[K, List[V]]] =
      f.foldRight(Some(Leaf(Nil)): Option[PropertyTree[K, List[V]]]) { (a, acc) =>
        acc.flatMap(value => a.map2[K, List[V], List[V]](value)(_ :: _))
      }

    tree match {
      case PropertyTree.Leaf(value) => Some(PropertyTree.Leaf(List(value)))
      case PropertyTree.Record(value) =>
        seqMap(
          value.toList
            .map(
              tuple =>
                tuple._1 -> {
                  sequence(tuple._2)
                }
            )
            .toMap
        ).map(PropertyTree.Record(_))

      case PropertyTree.Sequence(value) => convertList(value)
    }
  }

  def orElseEither[K, E1, E2, E3, A, B](
    tree1: PropertyTree[K, Either[E1, A]],
    tree2: PropertyTree[K, Either[E2, B]]
  )(f: (E1, E2) => E3): Option[PropertyTree[K, Either[E3, Either[A, B]]]] =
    tree1.map2(tree2)(
      (a, b) =>
        a match {
          case Left(error1) =>
            b match {
              case Left(error2) => Left(f(error1, error2))
              case Right(value) => Right(Right(value))
            }
          case Right(value) => Right(Left(value))
        }
    )

  def orElse[K, E1, E2, E3, A, B](
    tree1: PropertyTree[K, Either[E1, A]],
    tree2: PropertyTree[K, Either[E2, A]]
  )(
    f: (E1, E2) => E3
  ): Option[PropertyTree[K, Either[E3, A]]] =
    tree1.map2(tree2)(
      (a, b) =>
        a match {
          case Left(error1) =>
            b match {
              case Left(error2) => Left(f(error1, error2))
              case Right(value) => Right(value)
            }
          case Right(value) => Right(value)
        }
    )
}
