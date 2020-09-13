package zio.config

import scala.collection.immutable.Nil
import scala.annotation.tailrec

sealed trait PropertyTree[+K, +V] { self =>
  import PropertyTree._

  final def flatten[K1 >: K, V1 >: V]: Map[Vector[K1], ::[V1]] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], ::[V1]]): Map[Vector[K1], ::[V1]] =
      propertyTree match {
        case Empty           => acc
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v) =>
          acc
            .get(key)
            .fold[Map[Vector[K1], ::[V1]]](acc.updated(key, ::(v, Nil)))(
              value => acc.updated(key, ::(value.head, value.tail :+ v))
            )
        case Record(value) => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], ::[V1]])
  }

  final def flattenKeyAndValue[K1 >: K, V1 >: V](
    pathDelimiter: String = ".",
    valueDelimiter: String = ":"
  )(implicit KS: K1 =:= String): Map[String, String] =
    self
      .flatten[K1, V1]
      .map({ case (key, value) => (key.map(KS).mkString(pathDelimiter), value.mkString(valueDelimiter)) })

  final def flattenKeyWith[K1 >: K, V1 >: V](f: K1 => String)(
    appendPath: String
  ): Map[String, ::[V1]] =
    self.flatten[K1, V1].map({ case (key, value) => (key.map(f).mkString(appendPath), value) })

  final def flattenString[K1 >: K, V1 >: V](
    pathDelimiter: String = "."
  )(implicit KS: K1 =:= String): Map[String, ::[V1]] =
    flattenKeyWith[K1, V1](KS)(pathDelimiter)

  final def getOrElse[K1 >: K, V1 >: V](tree: => PropertyTree[K1, V1]): PropertyTree[K1, V1] =
    if (self == PropertyTree.empty) tree else self

  @tailrec
  final def getPath[K1 >: K](k: List[K1]): PropertyTree[K1, V] =
    k match {
      case Nil => self
      case head :: next =>
        self match {
          case Empty | Leaf(_) | Sequence(_) => Empty
          case Record(value) =>
            value.get(head.asInstanceOf[K]) match {
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

  final def map[V2](f: V => V2): PropertyTree[K, V2] = self match {
    case Leaf(value)     => Leaf(f(value))
    case Record(v)       => Record(v.map { case (k, tree) => (k, tree.map(f)) })
    case Sequence(value) => Sequence(value.map(_.map(f)))
    case Empty           => Empty
  }

  final def mapEmpty[K1 >: K, V1 >: V](f: Vector[Either[Int, K1]] => PropertyTree[K1, V1]): PropertyTree[K1, V1] = {
    def loop(acc: Vector[Either[Int, K1]], tree: PropertyTree[K, V]): PropertyTree[K1, V1] =
      tree match {
        case Leaf(value) => Leaf(value)
        case Record(value) =>
          Record(
            value
              .map(
                a =>
                  a._1.asInstanceOf[K1] -> loop(
                    acc :+ Right(a._1.asInstanceOf[K1]),
                    a._2.asInstanceOf[PropertyTree[K, V]]
                  )
              )
          )
        case PropertyTree.Empty => f(acc)
        case Sequence(value) =>
          Sequence(value.zipWithIndex.map {
            case (v, index) => loop(acc :+ Left(index), v)
          })
      }

    loop(Vector.empty, self)
  }

  final def mapEmptyToError[E, V2](f: => E): PropertyTree[K, Either[E, V]] = self match {
    case Leaf(value)        => Leaf(Right(value))
    case Record(v)          => Record(v.map { case (k, tree) => (k, tree.mapEmptyToError(f)) })
    case PropertyTree.Empty => PropertyTree.Leaf(Left(f))
    case Sequence(value)    => Sequence(value.map(_.mapEmptyToError(f)))
  }

  /**
   * Fix me
   * merge empty = self
   * merge self = ??? (uncertain)
   *
   * @param that
   * @return
   */
  final def merge[K1 >: K, V1 >: V](that: PropertyTree[K1, V1]): List[PropertyTree[K1, V1]] =
    (self, that) match {
      case (Sequence(l), Sequence(r)) => singleton(Sequence(l ++ r))
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
      case (left, right) if left.isEmpty  => singleton(right)
      case (left, right) if right.isEmpty => singleton(left)
      case (l, r)                         => l :: r :: Nil
    }

  final def reduceInner[V1 >: V](f: (V1, V1) => V1): PropertyTree[K, V1] = {
    def pruneEmpty[K, V](list: List[PropertyTree[K, V]]): List[PropertyTree[K, V]] =
      list.collect {
        case tree if !tree.isEmpty => tree
      }

    self match {
      case Empty       => Empty
      case Leaf(value) => Leaf(value)
      case Record(value) =>
        Record(value.mapValues(_.reduceInner(f)).toMap)

      case Sequence(value) =>
        val (vs0, rest0) = PropertyTree.partitionWith(value) {
          case Leaf(value) => value
        }

        val (vs, rest) = (vs0, pruneEmpty(rest0))

        //Fixme what should happen if we have Sequence(Nil). We don't know whether or not we should do at this level, or at a deeper level, which we don't know
        (vs, rest) match {
          case (Nil, _) =>
            Sequence(value.map(_.reduceInner(f)))
          case (vs, Nil) =>
            vs.reduceOption(f).map(Leaf(_)).getOrElse(Sequence(Nil))
          case (vs, res) =>
            Sequence(vs.map(Leaf(_))).zipWith(Sequence(res).reduceInner(f))(f)
        }
    }
  }

  final def zip[K1 >: K, V2, V3](that: PropertyTree[K1, V2]): PropertyTree[K1, (V, V2)] =
    self.zipWith(that)((a, b) => ((a, b)))

  final def zipWith[K1 >: K, V2, V3](that: PropertyTree[K1, V2])(f: (V, V2) => V3): PropertyTree[K, V3] =
    (self, that) match {
      case (Sequence(ls), Sequence(rs)) =>
        val max = ls.length.max(rs.length)
        val min = ls.length.min(rs.length)
        val pad = List.fill(max - min)(Empty)

        Sequence(
          (ls ++ pad)
            .zip(rs ++ pad)
            .map {
              case (l, r) => l.zipWith(r)(f)
            }
        )

      case (l: Record[K, V] @unchecked, r: Record[K, V2] @unchecked) =>
        Record((l.value.keySet ++ r.value.keySet).foldLeft(Map.empty[K, PropertyTree[K, V3]]) {
          case (map, key) =>
            val v  = l.value.getOrElse(key.asInstanceOf[K], Empty)
            val v2 = r.value.getOrElse(key.asInstanceOf[K], Empty)

            map.updated(key, v.zipWith(v2)(f))
        })

      case (Leaf(l), Leaf(r)) => Leaf(f(l, r))

      case (Sequence(l), r) => Sequence(l.map(_.zipWith(r)(f)))

      case (l, Sequence(r)) =>
        Sequence(r.map(tree => l.zipWith(tree)(f)))

      case (l, r: Record[K, V2] @unchecked) => Record(r.value.mapValues(v => l.zipWith(v)(f)).toMap)

      case (Record(l), r) => Record(l.mapValues(v => v.zipWith(r)(f)).toMap)

      case (Empty, _) => Empty

      case (_, Empty) => Empty
    }
}

object PropertyTree {

  final case class Leaf[V](value: V) extends PropertyTree[Nothing, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  case object Empty extends PropertyTree[Nothing, Nothing]

  final case class Sequence[K, V](value: List[PropertyTree[K, V]]) extends PropertyTree[K, V]

  val empty: PropertyTree[Nothing, Nothing] = Empty

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[V](t._2)))

  def fromStringMap(
    map: Map[String, String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): List[PropertyTree[String, String]] =
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

  def unflatten[K, V](key: List[K], value: ::[V]): PropertyTree[K, V] =
    unflatten(key, Sequence(value.map(Leaf(_))))

  def unflatten[K, V](key: List[K], tree: PropertyTree[K, V]): PropertyTree[K, V] =
    key match {
      case ::(head, next) => Record(Map(head -> unflatten(next, tree)))
      case Nil            => tree
    }

  def unflatten[K, V](map: Map[Vector[K], ::[V]]): List[PropertyTree[K, V]] =
    mergeAll(map.toList.map(tuple => unflatten(tuple._1.toList, tuple._2)))

  def mergeAll[K, V](list: List[PropertyTree[K, V]]): List[PropertyTree[K, V]] = list.reverse match {
    case Nil => Nil
    case head :: tail =>
      tail.foldLeft(List(head)) {
        case (acc, tree) => acc.flatMap(tree0 => tree.merge(tree0))
      }
  }

  def partitionWith[K, V, A](
    trees: List[PropertyTree[K, V]]
  )(pf: PartialFunction[PropertyTree[K, V], A]): (List[A], List[PropertyTree[K, V]]) =
    trees.map {
      case tree if pf.isDefinedAt(tree) => (pf(tree) :: Nil, Nil)
      case tree                         => (Nil, tree :: Nil)
    }.foldLeft((List.empty[A], List.empty[PropertyTree[K, V]])) {
      case ((accLeft, accRight), (left, right)) => (accLeft ++ left, accRight ++ right)
    }
}
