package zio.config

import zio.config.PropertyTree._

import scala.annotation.tailrec
import scala.collection.immutable.Nil

sealed trait PropertyTree { self =>
  final def ++(that: PropertyTree): PropertyTree =
    (self, that) match {
      case (Sequence(l), Sequence(r)) => Sequence(l ++ r)
      case (l, Sequence(r))           => Sequence(l :: r)
      case (Sequence(l), r)           => Sequence(l ::: List(r))
      case (l, r)                     => Sequence(l :: r :: Nil)
    }

  final def condense: PropertyTree =
    self match {
      case Leaf(value)    => Leaf(value)
      case Record(values) => Record(values.mapValues(_.condense).toMap)
      case Empty          => Empty
      case Sequence(values) =>
        PropertyTree.partitionWith(values) {
          case Record(value) => value
        } match {
          case (Nil, rs) => Sequence(rs.map(_.condense))
          case (ls, Nil) =>
            Record(ls.foldLeft(Map.empty[String, PropertyTree]) {
              case (acc, map) =>
                map.foldLeft(acc) {
                  case (acc, (k, v)) =>
                    acc.updated(k, acc.get(k).fold(v)(_ ++ v))
                }
            })

          case (ls, rs) =>
            Sequence(Record(ls.foldLeft(Map.empty[String, PropertyTree]) {
              case (acc, map) =>
                map.foldLeft(acc) {
                  case (acc, (k, v)) =>
                    acc.updated(k, acc.get(k).fold(v)(_ ++ v))
                }
            }) :: rs.map(_.condense))
        }

    }
  final def flatten: Map[Vector[String], ::[String]] = {
    def go(
      key: Vector[String],
      propertyTree: PropertyTree,
      acc: Map[Vector[String], ::[String]]
    ): Map[Vector[String], ::[String]] =
      propertyTree match {
        case Empty           => acc
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v)         => acc.updated(key, ::(v, Nil))
        case Record(value)   => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[String], ::[String]])
  }

  final def flattenKeyAndValue(
    pathDelimiter: String = ".",
    valueDelimiter: String = ":"
  ): Map[String, String] =
    self.flatten
      .map({ case (key, value) => (key.mkString(pathDelimiter), value.mkString(valueDelimiter)) })

  private def flattenKeyWith(f: String => String)(
    appendPath: String
  ): Map[String, ::[String]] =
    self.flatten.map({ case (key, value) => (key.map(f).mkString(appendPath), value) })

  final def flattenString(
    pathDelimiter: String = "."
  ): Map[String, ::[String]] =
    flattenKeyWith(identity)(pathDelimiter)

  final def getOrElse(tree: => PropertyTree): PropertyTree =
    if (self == PropertyTree.empty) tree else self

  @tailrec
  final def getPath(k: List[String]): PropertyTree =
    k match {
      case Nil => self
      case head :: next =>
        self match {
          case Empty | Leaf(_) | Sequence(_) => Empty
          case Record(value) =>
            value.get(head) match {
              case Some(value) => value.getPath(next)
              case None        => Empty
            }
        }
    }

  final def isEmpty: Boolean = self match {
    case Empty           => true
    case Leaf(_)         => false
    case Record(value)   => value.values.forall(_.isEmpty)
    case Sequence(value) => value.forall(_.isEmpty)
  }

  /**
   * Fix me
   * merge empty = self
   * merge self = ??? (uncertain)
   *
   * @param that
   * @return
   */
  final def merge(that: PropertyTree): List[PropertyTree] =
    (self, that) match {
      case (left, right) if left.isEmpty  => singleton(right)
      case (left, right) if right.isEmpty => singleton(left)
      case (Sequence(l), Sequence(r))     => singleton(Sequence(l ++ r))
      case (l: Record, r: Record) =>
        (l.value.keySet ++ r.value.keySet)
          .foldLeft(List[Map[String, PropertyTree]](Map.empty)) {
            case (acc, k) =>
              (l.value.get(k), r.value.get(k)) match {
                case (None, None) => acc
                case (Some(l), Some(r)) =>
                  l.merge(r).map(tree => (k, tree)).flatMap(tuple => acc.map(map => map + tuple))
                case (Some(l), None) => acc.map(map => map + (k -> l))
                case (None, Some(r)) => acc.map(map => map + (k -> r))
              }
          }
          .map(v => PropertyTree.Record(v))
      case (l, r) => l :: r :: Nil
    }

}

object PropertyTree {

  final case class Leaf(value: String) extends PropertyTree

  final case class Record(value: Map[String, PropertyTree]) extends PropertyTree

  case object Empty extends PropertyTree

  final case class Sequence(value: List[PropertyTree]) extends PropertyTree

  val empty: PropertyTree = Empty

  def fromMap(map: Map[String, String]): PropertyTree = Record(map.map(t => t._1 -> Leaf(t._2)))

  def fromStringMap(
    map: Map[String, String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): List[PropertyTree] =
    unflatten(
      map.map(
        tuple => {
          val vectorOfKeys = keyDelimiter match {
            case Some(keyDelimiter) => tuple._1.split(keyDelimiter).toVector.filterNot(_.trim == "")
            case None               => Vector(tuple._1)
          }
          vectorOfKeys ->
            (valueDelimiter.fold(List(tuple._2))(
              delim =>
                tuple._2
                  .split(delim)
                  .toList
            ) match {
              case h :: tail =>
                ::(h, tail)
              case Nil => singleton(tuple._2)
            })
        }
      )
    )

  def unflatten(key: List[String], value: ::[String]): PropertyTree =
    unflatten(key, Sequence(value.map(Leaf(_))))

  def unflatten(key: List[String], tree: PropertyTree): PropertyTree =
    key match {
      case ::(head, next) => Record(Map(head -> unflatten(next, tree)))
      case Nil            => tree
    }

  def unflatten(map: Map[Vector[String], ::[String]]): List[PropertyTree] =
    mergeAll(map.toList.map(tuple => unflatten(tuple._1.toList, tuple._2)))

  def mergeAll(list: List[PropertyTree]): List[PropertyTree] = list.reverse match {
    case Nil => Nil
    case head :: tail =>
      tail.foldLeft(List(head)) {
        case (acc, tree) => acc.flatMap(tree0 => tree.merge(tree0))
      }
  }

  private def partitionWith[A](
    trees: List[PropertyTree]
  )(pf: PartialFunction[PropertyTree, A]): (List[A], List[PropertyTree]) =
    trees.map {
      case tree if pf.isDefinedAt(tree) => (pf(tree) :: Nil, Nil)
      case tree                         => (Nil, tree :: Nil)
    }.foldLeft((List.empty[A], List.empty[PropertyTree])) {
      case ((accLeft, accRight), (left, right)) => (accLeft ++ left, accRight ++ right)
    }
}
