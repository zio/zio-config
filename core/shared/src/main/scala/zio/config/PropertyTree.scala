package zio.config

import com.github.ghik.silencer.silent
import zio.config.PropertyTreePath.Step

import scala.collection.immutable.Nil
import scala.util.Try
import scala.util.matching.Regex

final case class PropertyTreePath[K](path: Vector[Step[K]]) {
  def mapKeys(f: K => K): PropertyTreePath[K] =
    PropertyTreePath(path.map(_.map(f)))
}

object PropertyTreePath {

  sealed trait Step[+K] { self =>
    def map[K1 >: K, K2](f: K1 => K2): Step[K2] =
      self match {
        case Step.Index(n) => Step.Index(n)
        case Step.Value(k) => Step.Value(f(k.asInstanceOf[K1]))
      }
  }

  object Step {
    val pattern: Regex = """([a-zA-Z0-9 -@\-^-~]*)\[([0-9]*)\]""".r.anchored

    def steps[K](s: String)(implicit IsString: String =:= K): Vector[Step[K]] =
      Step.pattern
        .findAllIn(s)
        .matchData
        .flatMap(regexMatch =>
          (1 to regexMatch.groupCount).map { index =>
            val strOrInt = regexMatch.group(index)
            Try(Step.Index(strOrInt.toInt)).getOrElse(Step.Value(IsString(strOrInt)))
          }
        )
        .toVector

    final case class Index(n: Int)  extends Step[Nothing]
    final case class Value[K](k: K) extends Step[K]
  }

  /**
   * Example:
   *
   * $("aws.regions[0].`servers name`.plainText")
   *
   * @param path
   * @return
   */
  def $(path: String): PropertyTreePath[String] =
    PropertyTreePath(path.split(".").toVector.flatMap(str => Step.steps(str)))

  implicit def stringToPropTreePath(s: String): PropertyTreePath[String] = $(s)
}

@silent("Unused import")
sealed trait PropertyTree[+K, +V] { self =>
  import PropertyTree._
  import scala.collection.compat._

  /**
   * `at` allows us to fetch a sub-tree from property-tree
   *
   * Example:
   *
   * {{{
   *
   * Given a config:
   *
   *   {
   *    x : [ a, b, c ]
   *   }
   *
   *   at("x")            // returns Some([a, b, c])
   *   at("x").atIndex(2) // returns Some(Leaf("c"))
   *   at("x").atKey("y") // returns None
   *
   * Similarly, given a more complex config:
   *   {
   *    x : [
   *      {
   *        y1: 1
   *        y2: 2
   *        y3: 3
   *      }
   *      {
   *        y1: 1
   *        y2: 2
   *        y3: 3
   *      }
   *    ]
   *   }
   *
   *   at("x").atIndex(0).atKey("y1") // returns Some(Leaf(v1)
   * }}}
   */
  final def at[K1 >: K](propertyTreePath: PropertyTreePath[K1]): PropertyTree[K1, V] = {
    val steps = propertyTreePath.path

    steps.foldLeft(self) { (tree, step) =>
      (step match {
        case Step.Index(n) => tree.atIndex(n)
        case Step.Value(k) => tree.atKey(k.asInstanceOf[K])
      }).getOrElse(PropertyTree.empty)
    }
  }

  final def atKey[K1 >: K](key: K1): Option[PropertyTree[K1, V]] =
    self match {
      case Leaf(_)            => None
      case Record(value)      => value.get(key.asInstanceOf[K])
      case PropertyTree.Empty => None
      case Sequence(_)        => None
    }

  final def atIndex[K1 >: K](index: Int): Option[PropertyTree[K1, V]] =
    self match {
      case Leaf(_)            => None
      case Record(_)          => None
      case PropertyTree.Empty => None
      case Sequence(value)    => value.lift(index)
    }

  final def flatten[K1 >: K, V1 >: V]: Map[Vector[K1], ::[V1]] = {
    def go(key: Vector[K1], propertyTree: PropertyTree[K1, V], acc: Map[Vector[K1], ::[V1]]): Map[Vector[K1], ::[V1]] =
      propertyTree match {
        case Empty           => acc
        case Sequence(value) => value.foldLeft(acc)((acc, propertyTree) => go(key, propertyTree, acc))
        case Leaf(v)         =>
          acc
            .get(key)
            .fold[Map[Vector[K1], ::[V1]]](acc.updated(key, ::(v, Nil)))(value =>
              acc.updated(key, ::(value.head, value.tail :+ v))
            )
        case Record(value)   => value.flatMap(t => go(key :+ t._1, t._2, acc))
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
  ): Map[String, ::[V1]]                             =
    self.flatten[K1, V1].map({ case (key, value) => (key.map(f).mkString(appendPath), value) })

  final def flattenString[K1 >: K, V1 >: V](
    pathDelimiter: String = "."
  )(implicit KS: K1 =:= String): Map[String, ::[V1]] =
    flattenKeyWith[K1, V1](KS)(pathDelimiter)

  final def getOrElse[K1 >: K, V1 >: V](tree: => PropertyTree[K1, V1]): PropertyTree[K1, V1] =
    if (self == PropertyTree.empty) tree else self

  final def getPath[K1 >: K](k: List[K1]): PropertyTree[K1, V] =
    k.foldLeft(self)({ case (node, segment) =>
      node match {
        case Empty | Leaf(_) | Sequence(_) => Empty
        case record: Record[K, V]          =>
          record.value.get(segment.asInstanceOf[K]) match {
            case Some(value) => value
            case None        => Empty
          }
      }
    })

  final def isEmpty: Boolean = self match {
    case Empty           => true
    case Leaf(_)         => false
    case Record(value)   => value.values.isEmpty
    case Sequence(value) => value.forall(_.isEmpty)
  }

  final def map[V2](f: V => V2): PropertyTree[K, V2] = self match {
    case Leaf(value)     => Leaf(f(value))
    case Record(v)       => Record(v.map { case (k, tree) => (k, tree.map(f)) })
    case Sequence(value) => Sequence(value.map(_.map(f)))
    case Empty           => Empty
  }

  final def merge[K1 >: K, V1 >: V](that: PropertyTree[K1, V1]): List[PropertyTree[K1, V1]] =
    (self, that) match {
      case (Sequence(l), Sequence(r))           => singleton(Sequence(l ++ r))
      case (l: Record[K, V], r: Record[K1, V1]) =>
        (l.value.keySet ++ r.value.keySet)
          .foldLeft(List[Map[K1, PropertyTree[K1, V1]]](Map.empty)) { case (acc, k) =>
            (l.value.get(k.asInstanceOf[K]), r.value.get(k)) match {
              case (None, None)       => acc
              case (Some(l), Some(r)) =>
                l.merge(r).map(tree => (k, tree)).flatMap(tuple => acc.map(map => map + tuple))
              case (Some(l), None)    => acc.map(map => map + (k -> l))
              case (None, Some(r))    => acc.map(map => map + (k -> r))
            }
          }
          .map(v => PropertyTree.Record(v))
      case (left, right) if left.isEmpty        => singleton(right)
      case (left, right) if right.isEmpty       => singleton(left)
      case (l, r)                               => l :: r :: Nil
    }

  final def nonEmpty: Boolean = !isEmpty
}

object PropertyTree {
  def apply[V](v: V): PropertyTree[Nothing, V] =
    Leaf(v)

  private[config] final case class Leaf[V](value: V) extends PropertyTree[Nothing, V]

  private[config] final case class Record[K, V](value: Map[K, PropertyTree[K, V]]) extends PropertyTree[K, V]

  private[config] case object Empty extends PropertyTree[Nothing, Nothing]

  private[config] final case class Sequence[K, V](value: List[PropertyTree[K, V]]) extends PropertyTree[K, V]

  val empty: PropertyTree[Nothing, Nothing] = Empty

  def fromMap[K, V](map: Map[K, V]): PropertyTree[K, V] =
    Record(map.map(t => t._1 -> Leaf[V](t._2)))

  def fromStringMap(
    map: Map[String, String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): List[PropertyTree[String, String]]                 =
    unflatten(
      map.map { tuple =>
        val vectorOfKeys = keyDelimiter match {
          case Some(keyDelimiter) => tuple._1.split(keyDelimiter).toVector.filterNot(_.trim == "")
          case None               => Vector(tuple._1)
        }
        vectorOfKeys ->
          (valueDelimiter.fold(List(tuple._2))(delim =>
            tuple._2
              .split(delim)
              .toList
          ) match {
            case h :: tail =>
              ::(h, tail)
            case Nil       => singleton(tuple._2)
          })
      }
    )

  def mergeAll[K, V](list: List[PropertyTree[K, V]]): List[PropertyTree[K, V]] = list.reverse match {
    case Nil          => Nil
    case head :: tail =>
      tail.foldLeft(List(head)) { case (acc, tree) =>
        acc.flatMap(tree0 => tree.merge(tree0))
      }
  }

  def unflatten[K, V](key: List[K], value: ::[V]): PropertyTree[K, V] =
    unflatten(key, Sequence(value.map(Leaf(_))))

  def unflatten[K, V](key: List[K], tree: PropertyTree[K, V]): PropertyTree[K, V] =
    key match {
      case ::(head, next) => Record(Map(head -> unflatten(next, tree)))
      case Nil            => tree
    }

  def unflatten[K, V](map: Map[Vector[K], ::[V]]): List[PropertyTree[K, V]] =
    mergeAll(map.toList.map(tuple => unflatten(tuple._1.toList, tuple._2)))
}
