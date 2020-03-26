package zio.config

import scala.collection.immutable.Nil
import PropertyTree._
import zio.config.ReadError.{ FormatError, MissingValue }

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
            val v  = l.value.getOrElse(key.asInstanceOf[K], Empty)
            val v2 = r.value.getOrElse(key.asInstanceOf[K], Empty)

            map.updated(key, v.zipWith(v2)(f))
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

      case (Empty, _) => Empty

      case (_, Empty) => Empty
    }

  final def zip[K1 >: K, V2, V3](that: PropertyTree[K1, V2]): PropertyTree[K1, (V, V2)] =
    self.zipWith(that)((a, b) => ((a, b)))

  final def transformSome[K1 >: K, V1 >: V](
    f: PartialFunction[PropertyTree[K1, V1], PropertyTree[K1, V1]]
  ): PropertyTree[K1, V1] =
    self match {
      case x @ Leaf(_) => f.lift(x).getOrElse(x)
      case x @ Record(value) =>
        val r = Record(value.mapValues(_.transformSome(f)).toMap[K1, PropertyTree[K1, V1]])
        f.lift(r).getOrElse(r)
      case x @ PropertyTree.Empty => f.lift(x).getOrElse(x)
      case x @ Sequence(value) =>
        val s = Sequence(value.map(_.transformSome(f)))
        f.lift(s).getOrElse(s)
    }

  def mapEmpty[K1 >: K, V1 >: V](f: Vector[Either[Int, K1]] => PropertyTree[K1, V1]): PropertyTree[K1, V1] = {
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

  def reduceInner[V1 >: V](zero: V1)(f: (V1, V1) => V1): PropertyTree[K, V1] = {
    // def flatten[A, B](tuple: (List[(List[A], List[B])], List[B])): (List[A], List[B]) =
    //   (tuple._1.flatMap(_._1), tuple._1.flatMap(_._2) ++ tuple._2)

    def pruneEmpty[K, V](list: List[PropertyTree[K, V]]): List[PropertyTree[K, V]] =
      list.collect {
        case tree if !tree.isEmpty => tree
      }

    self match {
      case Empty         => Empty
      case Leaf(value)   => Leaf(value)
      case Record(value) => Record(value.mapValues(_.reduceInner(zero)(f)).toMap)
      //               Sequence(List()), // It should have been Leaf(Right(Nil))
      case Sequence(value) if value.isEmpty => Leaf(zero)
      case Sequence(value) =>
        val (vs0, rest0) = PropertyTree.partitionWith(zero, value) {
          case Leaf(value) => value
        }

        val (vs, rest) = (vs0, pruneEmpty(rest0))

        (vs, rest) match {
          case (Nil, _)  => Sequence(value.map(_.reduceInner(zero)(f)))
          case (vs, Nil) => vs.reduceOption(f).map(Leaf(_)).getOrElse(Sequence(Nil))
          case (vs, _)   => Sequence(vs.map(Leaf(_)))

        }
    }
  }

  def getPath2[K1 >: K](k: List[K1]): List[PropertyTree[K1, V]] =
    k match {
      case head :: Nil =>
        self match {
          case Leaf(value)     => Empty :: Nil
          case Record(value)   => value.get(head.asInstanceOf[K]).getOrElse(Empty) :: Nil
          case Sequence(value) => value
          case Empty           => Empty :: Nil

        }
      case Nil => self :: Nil
      case head :: next =>
        self match {
          case Empty         => Empty :: Nil
          case Leaf(_)       => Empty :: Nil
          case Record(value) => value.get(head.asInstanceOf[K]).map(_.getPath2(next)).getOrElse(Empty :: Nil)
          case Sequence(r)   => Sequence(r.map(_.getPath2(k)).flatten) :: Nil
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
      case (l, r) => l :: r :: Nil
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

  def partitionWith[K, V, A](
    zero: A,
    trees: List[PropertyTree[K, V]]
  )(pf: PartialFunction[PropertyTree[K, V], A]): (List[A], List[PropertyTree[K, V]]) =
    if (trees.isEmpty) (List(zero), Nil)
    else
      trees.collect {
        case tree if pf.isDefinedAt(tree) => (pf(tree) :: Nil, Nil)
        case tree                         => (Nil, tree :: Nil)
      }.foldLeft((List.empty[A], List.empty[PropertyTree[K, V]])) {
        case ((accLeft, accRight), (left, right)) => (accLeft ++ left, accRight ++ right)
      }

  def sequence[K, V](tree: List[PropertyTree[K, V]]): PropertyTree[K, List[V]] =
    tree match {
      case ::(head, next) => head.zipWith(sequence(next))(_ :: _)
      case Nil            => Leaf(Nil)
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

  final def transformErrors[K1, V1, E](
    propertyTree: PropertyTree[K1, Either[E, V1]],
    f: PartialFunction[PropertyTree[K1, Either[E, V1]], PropertyTree[K1, Either[E, V1]]]
  ): PropertyTree[K1, Either[E, V1]] =
    propertyTree match {
      case x @ Leaf(_) => f.lift(x).getOrElse(x)
      case x @ Record(value) =>
        val r: PropertyTree[K1, Either[E, V1]] = Record(
          value.mapValues(tree => transformErrors(tree, f)).toMap[K1, PropertyTree[K1, Either[E, V1]]]
        )
        f.lift(r).getOrElse(r)
      case x @ PropertyTree.Empty => f.lift(x).getOrElse(x)
      case x @ Sequence(value) =>
        val s = Sequence(value.map(tree => transformErrors(tree, f)))
        f.lift(s).getOrElse(s)
    }

  def getValue[K, V](propertyTree: PropertyTree[K, V]): V =
    propertyTree match {
      case Leaf(value)     => value
      case Record(value)   => getValue(value.toList.map(_._2).head)
      case Empty           => throw new Exception("bhoom")
      case Sequence(value) => getValue(value.head)
    }

}

object Example extends App {
  final case class Country(name: String, code: Int)
  final case class AccountIdDetails(country: List[Country])
  final case class AccountRegions(accountId: List[AccountIdDetails])
  final case class AwsDetails(accounts: List[AccountRegions])

  val listHocon =
    """
    accounts = [
      {
          
          accountId = [
            {
              country = [
                {
                  name : aus
                  code : 10
                }
              ]
            
            }
            
            {
              country = [
                {
                  name : sua  
                  code : 20
                }
              ]
            }
          ]
      }
      {
          accountId = [
            {
              country = [
                {
                  name : aus
                  code : 10
                }
              ]
            
            }
            
            {
              country = [
                {
                  name : sua
                  code : 20
                }
              ]
            }
          ]
      }
    ]
    """

  val tree =
    Record(
      Map(
        "accounts" -> Sequence(
          List(
            Record(
              Map(
                "accountId" -> Sequence(
                  List(
                    Record(Map("country" -> Sequence(List(Record(Map("code" -> Leaf("10"), "name" -> Leaf("aus"))))))),
                    Record(Map("country" -> Sequence(List(Record(Map("code" -> Leaf("20"), "name" -> Leaf("sua")))))))
                  )
                )
              )
            ),
            Record(
              Map(
                "accountId" -> Sequence(
                  List(
                    Record(Map("country" -> Sequence(List(Record(Map("code" -> Leaf("10"), "name" -> Leaf("aus"))))))),
                    Record(Map("country" -> Sequence(List(Record(Map("code" -> Leaf("20"), "name" -> Leaf("sua")))))))
                  )
                )
              )
            )
          )
        )
      )
    )

  // The result of this output is
  Right(
    AwsDetails(
      List(
        AccountRegions(
          List(
            AccountIdDetails(List(Country("aus", 10), Country("aus", 10))),
            AccountIdDetails(List(Country("sua", 20), Country("sua", 20)))
          )
        )
      )
    )
  )

  // Think this is coz

  // Step 1
  val countryTree = tree.getPath(List("accounts", "accountId", "country", "name"))

  println("get path " + countryTree)

  // Sequence(
  //   List(
  //     Sequence(List(Sequence(List(Leaf(aus))), Sequence(List(Leaf(sua))))),
  //     Sequence(List(Sequence(List(Leaf(aus))), Sequence(List(Leaf(sua)))))
  //   )
  // )

  // Sequence(
  //   List(
  //    Leaf(List(aus, sua)),
  //    Leaf(List(aus, sua))
  //   )
  // )

  // Sequence(List(Sequence(List(Sequence(List(Leaf(aus), Leaf(sua)))), Sequence(List(Sequence(List(Leaf(aus), Leaf(sua)))))))
  //println("result is " + countryTree.map(_ :: Nil).reduceInner(_ ++ _))

  // Sequence(
  //   List(
  //     Sequence(List(Leaf(List(aus)))),
  //     Sequence(List(Leaf(List(sua)))),
  //     Sequence(List(Leaf(List(aus)))),
  //     Sequence(List(Leaf(List(sua))))
  //   )
  // )

  // Step 2: Sequence this PropertyTree, and that's mindbending
  // To narrow dow the example:

  /*  val input: PropertyTree[String, Either[ReadError[String], String]] =
    Sequence(
      List(
        Sequence(
          List(
            Sequence(
              List(
                Leaf(Left(ReadError.MissingValue(Vector(Right("a"))))),
                Leaf(Left(ReadError.MissingValue(Vector(Right("b"))))),
                Sequence(List(Leaf(Right("1")), Leaf(Right("2")), Leaf(Right("3"))))
              )
            )
          )
        )
      )
    )*/

  /*  val transformedErrors =
    PropertyTree.transformErrors[String, String, ReadError[String]](input, {
      case PropertyTree.Leaf(Left(value)) => PropertyTree.Sequence(List(PropertyTree.Leaf(Left(value))))
    })

  println(transformedErrors.map(_.map(_ :: Nil)).reduceInner[Either[ReadError[String], List[String]]] {
    case (Right(l), Right(r)) => Right(l ++ r)
    case (Left(l), Right(_))  => Left(l)
    case (Right(_), Left(r))  => Left(r)
    case (Left(l), Left(r))   => Left(ReadError.AndErrors(l :: r :: Nil))
  })*/

  val anotherInput: PropertyTree[String, Either[ReadError[String], String]] =
    Sequence(
      List(
        Sequence(
          List(
            Sequence(
              List(
                Sequence(
                  List(
                    Leaf(
                      Left(
                        MissingValue(Vector(Right("export-details")))
                      )
                    )
                  )
                ),
                Sequence(
                  List(
                    Leaf(
                      Left(
                        MissingValue(Vector(Right("export-details")))
                      )
                    )
                  )
                ),
                Sequence(
                  List(
                    Leaf(Right("1")),
                    Leaf(Right("2")),
                    Leaf(
                      Left(
                        FormatError(
                          Vector(Right("export-details")),
                          "Provided value is xxx, expecting the type int"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  val anotherTransformedErrors =
    PropertyTree.transformErrors[String, String, ReadError[String]](anotherInput, {
      case PropertyTree.Leaf(Left(value)) => PropertyTree.Sequence(List(PropertyTree.Leaf(Left(value))))
    })

//  Sequence(
//    List(
//      Sequence(
//        List(
//          Sequence(
//            List(
//              Sequence(List()),
//              Sequence(
//                List(
//                  Leaf(
//                    Left(
//                      MissingValue(Vector(Right("export-details"), Right("extra-details"), Right("r"), Right("vvv")))
//                    )
//                  )
//                )
//              ),
//              Sequence(List(Leaf(Right(1)), Leaf(Right(2)), Leaf(Right(3))))
//            )
//          )
//        )
//      )
//    )
//  )
//
//  Sequence(
//    List(
//      Sequence(
//        List(
//          Sequence(
//            List(
//              Sequence(List()), // It should have been Leaf(Right(Nil))
//              Leaf(Left(MissingValue(Vector(Right("export-details"), Right(extra - details), Right(r), Right(vvv))))),
//              Leaf(Right(List(1, 2, 3)))
//            )
//          )
//        )
//      )
//    )
//  )
//  println(anotherTransformedErrors.map(_.map(_ :: Nil)).reduceInner[Either[ReadError[String], List[String]]] {
//    case (Right(l), Right(r)) => Right(l ++ r)
//    case (Left(l), Right(_))  => Left(l)
//    case (Right(_), Left(r))  => Left(r)
//    case (Left(l), Left(r))   => Left(ReadError.AndErrors(l :: r :: Nil))
//  })

}
