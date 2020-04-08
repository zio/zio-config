package zio.config

import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.random.Random
import zio.test.{ Gen, Sized }

object PropertyTreeTestUtils {
  final case class PropertyTreeTestParams(
    nestedSequencesCount: Int,
    recordKeyCount: Int
  )

  private[config] val genLeaf: Gen[Random with Sized, Leaf] =
    Gen.anyString.map(str => Leaf(str))

  private[config] val genListOfLeaves: Gen[Random with Sized, List[Leaf]] =
    Gen.int(1, 20).flatMap(n => Gen.listOfN(n)(genLeaf))

  final case class Leaves(list: List[List[Leaf]]) {
    def map(f: String => String): Leaves = Leaves(list.map(_.map(t => Leaf(f(t.value)))))

    def toSequences: List[Sequence] =
      list.map(Sequence(_))
  }

  object Leaves {
    val genLeaves: Gen[Random with Sized, Leaves] =
      Gen.int(1, 3).flatMap(n => Gen.listOfN(n)(genListOfLeaves)).map(Leaves(_))
  }

  private[config] val nLevelSequenceWithRecords: Gen[Random with Sized, (Record, Leaves, PropertyTreeTestParams)] =
    for {
      leaves <- Leaves.genLeaves
      nested <- Gen.int(1, 3)
      keys   = 1 to 3
      map = keys.toList.foldLeft(Map.empty[String, PropertyTree]) { (acc, a) =>
        acc.updated(a.toString, generateAtLeastNLevelSequenceTree(nested, leaves.toSequences))
      }
    } yield (Record(map), leaves, PropertyTreeTestParams(nested, keys.size))

  private[config] val nLevelSequenceWithRecordsEmpty: Gen[Random with Sized, (Record, PropertyTreeTestParams)] =
    for {
      nested <- Gen.int(1, 3)
      keys   = 1 to 3
      map = keys.toList.foldLeft(Map.empty[String, PropertyTree]) { (acc, a) =>
        acc.updated(a.toString, generateAtLeastNLevelSequenceTree(nested, List(PropertyTree.empty)))
      }
    } yield (Record(map), PropertyTreeTestParams(nested, keys.size))

  private[config] val nLevelSequenceWithLeaves: Gen[Random with Sized, (PropertyTree, Leaves, Int)] =
    Leaves.genLeaves.flatMap(
      l =>
        Gen
          .int(1, 3)
          .map(
            n => (generateAtLeastNLevelSequenceTree(n, l.toSequences), l, n)
          )
    )

  private[config] def generateAtLeastNLevelSequenceTree(n: Int, inject: List[PropertyTree]): PropertyTree =
    if (n <= 1) {
      Sequence(inject)

    } else {
      Sequence(List(generateAtLeastNLevelSequenceTree(n - 1, inject)))
    }

  private[config] def getTreeFromNLevelSequence(tree: PropertyTree, n: Int): List[PropertyTree] =
    tree match {
      case Leaf(r)                   => List(Leaf(r))
      case Record(value)             => value.values.flatMap(tree => getTreeFromNLevelSequence(tree, n)).toList
      case PropertyTree.Empty        => Nil
      case Sequence(value) if n == 1 => value
      case Sequence(trees)           => trees.flatMap(tree => getTreeFromNLevelSequence(tree, n - 1))
    }
}
