package zio.config

import scala.collection.immutable.Nil
import PropertyTree._

sealed trait PropertyTree[+K, +V] { self =>
  final def map[V2](f: V => V2): PropertyTree[K, V2] = self match {
    case Leaf(value)     => Leaf(f(value))
    case Record(v)       => Record(v.map { case (k, tree) => (k, tree.map(f)) })
    case Sequence(value) => Sequence(value.map(_.map(f)))
    case Empty           => Empty
  }

  // Consider empty as an error
  def mapEmptyToError[E, V2](f: => E): PropertyTree[K, Either[E, V]] = self match {
    case Leaf(value)        => Leaf(Right(value))
    case Record(v)          => Record(v.map { case (k, tree) => (k, tree.mapEmptyToError(f)) })
    case PropertyTree.Empty => PropertyTree.Leaf(Left(f))
    case Sequence(value)    => Sequence(value.map(_.mapEmptyToError(f)))
  }

  final def zipWith[K1 >: K, V2, V3](that: PropertyTree[K1, V2])(f: (V, V2) => V3): PropertyTree[K, V3] =
    (self, that) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Sequence(ls), Sequence(rs)) =>
        val max = ls.length.max(rs.length)
        val min = ls.length.min(rs.length)
        val pad = List.fill(max - min)(Empty)

        val rsss = Sequence(
          (ls ++ pad)
            .zip(rs ++ pad)
            .map {
              case (l, r) => l.zipWith(r)(f)
            }
        )
        rsss

      case (l: Record[K, V] @unchecked, r: Record[K, V2] @unchecked) =>
        Record((l.value.keySet ++ r.value.keySet).foldLeft(Map.empty[K, PropertyTree[K, V3]]) {
          case (map, key) =>
            (for {
              v  <- l.value.get(key.asInstanceOf[K])
              v2 <- r.value.get(key.asInstanceOf[K])
            } yield v.zipWith(v2)(f)).fold(map)(v3 => map.updated(key, v3))
        })

      case (Leaf(l), Leaf(r)) =>
        Leaf(f(l, r))
      case (Sequence(l), r) =>
        Sequence(l.map(_.zipWith(r)(f)))

      case (l, Sequence(r)) =>
        Sequence(r.map(tree => l.zipWith(tree)(f)))

      case (l, r: Record[K, V2] @unchecked) =>
        Record(r.value.mapValues(v => l.zipWith(v)(f)).toMap)

      case (Record(l), r) =>
        Record(l.mapValues(v => v.zipWith(r)(f)).toMap)
    }

  final def zip[K1 >: K, V2, V3](that: PropertyTree[K1, V2]): PropertyTree[K1, (V, V2)] =
    self.zipWith(that)((a, b) => ((a, b)))

  final def flatten[K1 >: K, V1 >: V]: Map[Vector[K1], ::[V1]] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], ::[V1]]): Map[Vector[K1], ::[V1]] =
      propertyTree match {
        case Empty           => acc
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v)         => acc.updated(key, ::(v, Nil))
        case Record(value)   => value.flatMap(t => go(key :+ t._1, t._2, acc))
      }

    go(Vector.empty, self, Map.empty[Vector[K1], ::[V1]])
  }

  def isEmpty: Boolean = self match {
    case Empty           => true
    case Leaf(_)         => false
    case Record(value)   => value.values.forall(_.isEmpty)
    case Sequence(value) => value.forall(_.isEmpty)
  }

  def getPath[K1 >: K](k: List[K1]): PropertyTree[K1, V] =
    k match {
      case Nil => self
      case head :: next =>
        self match {
          case Empty         => Empty
          case Leaf(_)       => Empty
          case Record(value) => value.get(head.asInstanceOf[K]).map(_.getPath(next)).getOrElse(Empty)
          case Sequence(r)   => Sequence(r.map(_.getPath(k)))
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
    pathDelimiter: String = "."
  )(implicit KS: K1 =:= String): Map[String, ::[V1]] =
    flattenKeyWith[K1, V1](KS)(pathDelimiter)

  final def flattenKeyWith[K1 >: K, V1 >: V](f: K1 => String)(
    appendPath: String
  ): Map[String, ::[V1]] =
    self.flatten[K1, V1].map({ case (key, value) => (key.map(f).mkString(appendPath), value) })

  final def flattenKeyAndValue[K1 >: K, V1 >: V](
    pathDelimiter: String = ".",
    valueDelimiter: String = ":"
  )(implicit KS: K1 =:= String): Map[String, String] =
    self
      .flatten[K1, V1]
      .map({ case (key, value) => (key.map(KS).mkString(pathDelimiter), value.mkString(valueDelimiter)) })
}

object PropertyTree {

  final case class Leaf[V](value: V) extends PropertyTree[Nothing, V]

  final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  case object Empty extends PropertyTree[Nothing, Nothing]

  final case class Sequence[K, V](value: List[PropertyTree[K, V]]) extends PropertyTree[K, V]

  val empty: PropertyTree[Nothing, Nothing] = Empty

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

  def sequence[K, V](tree: List[PropertyTree[K, V]]): PropertyTree[K, List[V]] =
    tree.foldRight(Leaf(Nil): PropertyTree[K, List[V]]) { (a, acc) =>
      a.zipWith[K, List[V], List[V]](acc)(_ :: _)
    }

  def sequence[K, V](tree: PropertyTree[K, V]): PropertyTree[K, List[V]] = {
    //println(tree)
    val result = tree match {
      case Leaf(value) =>
        Leaf(singleton(value))
      case Record(value) =>
        Record(value.foldLeft(Map.empty[K, PropertyTree[K, List[V]]]) { (acc, a) =>
          acc
            .get(a._1)
            .map(_.zipWith(sequence(a._2)) { (v1, v2) =>
              v1 ++ v2
            })
            .map(tree => acc.updated(a._1, tree))
            .getOrElse(acc.updated(a._1, a._2.map(singleton)))
        })
      case Empty => Empty
      case Sequence(value) =>
        def loop(acc: PropertyTree[K, List[V]], rest: List[PropertyTree[K, V]]): PropertyTree[K, List[V]] =
          rest match {
            case h1 :: Nil =>
              sequence(h1)
            case Nil => acc
            case h1 :: h2 :: h3 =>
              (h1, h2) match {
                case (l: Record[K, V], r: Record[K, V]) =>
                  val zipped = l.zipWith(r)((a, b) => List(a, b))
                  // println("sequenced " + zipped)
                  val finall = loop(acc.zipWith(zipped)((a, b) => a ++ b), h3)
                  // println(s"the finall is ${acc} and ${zipped} and ${finall}")
                  finall
                case (l: Leaf[V], r: Leaf[V]) =>
                  // println("what?")
                  loop(acc.zipWith(Leaf(List(l.value, r.value)))((a, b) => a ++ b), h3)
                case (Sequence(v1), Sequence(v2)) =>
                  // println("first one " + v1 + "   " + loop(acc, v1))
                  // println("second one " + v2 + "   " + loop(acc, v2))

                  Sequence(List(loop(acc, v1), (loop(acc, v2))))
                case (Empty, Empty) =>
                  //println("what? xx")
                  loop(acc, h3)

                case _ => throw new Exception("fucked up")
              }
          }

        println(s"the input value is ${value}")
        val output = loop(PropertyTree.Leaf(Nil), value)
        println(s"the output value is ${value}")
    }

    //println(result)
    result
  }

  def orElseEither[K, E1, E2, E3, A, B](
    tree1: PropertyTree[K, Either[E1, A]],
    tree2: PropertyTree[K, Either[E2, B]]
  )(f: (E1, E2) => E3): PropertyTree[K, Either[E3, Either[A, B]]] =
    tree1.zipWith(tree2)(
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
  ): PropertyTree[K, Either[E3, A]] =
    tree1.zipWith(tree2)(
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

object Main extends App {
  val input =
    Sequence(
      List(
        Sequence(
          List(
            Sequence(
              List(
                Sequence(
                  List(
                    Record(Map("vvv" -> Leaf(Right(1)))),
                    Record(Map("vvv" -> Leaf(Right(2)))),
                    Record(Map("vvv" -> Leaf(Right(3))))
                  )
                ),
                Sequence(
                  List(
                    Record(Map("vvv" -> Leaf(Right(1)))),
                    Record(Map("vvv" -> Leaf(Right(2)))),
                    Record(Map("vvv" -> Leaf(Right(3))))
                  )
                ),
                Sequence(
                  List(
                    Record(Map("vvv" -> Leaf(Right(1)))),
                    Record(Map("vvv" -> Leaf(Right(2)))),
                    Record(Map("vvv" -> Leaf(Right(3))))
                  )
                )
              )
            )
          )
        )
      )
    )

  val s = PropertyTree.sequence(input)

  println(s)

}
