package zio.config

import zio.config.PropertyTree.{Leaf, Record, Sequence}
import zio.random.Random
import zio.test.{Gen, Sized}

object PropertyTreeTestUtils {
  final case class PropertyTreeTestParams(
    nestedSequencesCount: Int,
    recordKeyCount: Int
  )

  private[config] val genLeaf: Gen[Random with Sized, Leaf[String]] =
    Gen.anyString.map(str => Leaf(str))

  private[config] val genListOfLeaves: Gen[Random with Sized, List[Leaf[String]]] =
    Gen.int(1, 20).flatMap(n => Gen.listOfN(n)(genLeaf))

  final case class Leaves[V](list: List[List[Leaf[V]]]) {
    def map[V1](f: V => V1): Leaves[V1]    = Leaves(list.map(_.map(t => Leaf(f(t.value)))))
    def sequenceInner: List[Leaf[List[V]]] =
      list.map { l =>
        Leaf(l.foldRight(List.empty[V]) { (a, acc) =>
          a.value :: acc
        })
      }

    def toSequences[K]: List[Sequence[K, V]] =
      list.map(Sequence[K, V](_))
  }

  object Leaves {
    val genLeaves: Gen[Random with Sized, Leaves[String]] =
      Gen.int(1, 3).flatMap(n => Gen.listOfN(n)(genListOfLeaves)).map(Leaves(_))
  }

  private[config] val nLevelSequenceWithRecords
    : Gen[Random with Sized, (Record[String, String], Leaves[String], PropertyTreeTestParams)] =
    for {
      leaves <- Leaves.genLeaves
      nested <- Gen.int(1, 3)
      keys    = (1 to 3)
      map     = keys.toList.foldLeft(Map.empty[String, PropertyTree[String, String]]) { (acc, a) =>
                  acc.updated(a.toString, generateAtleastNLevelSequenceTree(nested, leaves.toSequences))
                }
    } yield (Record(map), leaves, PropertyTreeTestParams(nested, keys.size))

  private[config] val nLevelSequenceWithRecordsEmpty
    : Gen[Random with Sized, (Record[String, String], PropertyTreeTestParams)] =
    for {
      nested <- Gen.int(1, 3)
      keys    = (1 to 3)
      map     = keys.toList.foldLeft(Map.empty[String, PropertyTree[String, String]]) { (acc, a) =>
                  acc.updated(a.toString, generateAtleastNLevelSequenceTree(nested, List(PropertyTree.empty)))
                }
    } yield (Record(map), PropertyTreeTestParams(nested, keys.size))

  private[config] val nLevelSequenceWithLeaves
    : Gen[Random with Sized, (PropertyTree[String, String], Leaves[String], Int)] =
    Leaves.genLeaves.flatMap(l =>
      Gen
        .int(1, 3)
        .map(n => (generateAtleastNLevelSequenceTree(n, l.toSequences), l, n))
    )

  private[config] def generateAtleastNLevelSequenceTree[A](
    n: Int,
    inject: List[PropertyTree[String, A]]
  ): PropertyTree[String, A] =
    if (n <= 1) {
      Sequence(inject)

    } else {
      Sequence(List(generateAtleastNLevelSequenceTree(n - 1, inject)))
    }

  private[config] def getTreeFromNLevelSequence[A](
    tree: PropertyTree[String, A],
    n: Int
  ): List[PropertyTree[String, A]] =
    tree match {
      case Leaf(r)                     => List(Leaf(r))
      case Record(value)               => value.values.flatMap(tree => getTreeFromNLevelSequence(tree, n)).toList
      case PropertyTree.Empty          => Nil
      case Sequence(value) if (n == 1) => value
      case Sequence(trees)             => trees.flatMap(tree => getTreeFromNLevelSequence(tree, n - 1))
    }
}
