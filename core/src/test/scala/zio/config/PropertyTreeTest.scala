package zio.config

import PropertyTreeTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import PropertyTree._

object PropertyTreeTest
    extends BaseSpec(
      suite("PropertyTree.reduceInner")(
        testM("reduceInner goes 4 level deep and sequence the Sequence(Leaves)") {
          check(generate4Level) { input =>
            val (tree, leaves) = input

            val reducedTo3Level: PropertyTree[String, List[String]] =
              tree.map(_ :: Nil).reduceInner(_ ++ _)

            assert(
              reducedTo3Level,
              equalTo(generate3Level(leaves.sequenceInner))
            )
          }
        }
      )
    )

object PropertyTreeTestUtils {
  private[config] val genLeaf: Gen[Random with Sized, Leaf[String]] =
    Gen.anyString.map(str => Leaf(str))

  private[config] val genListOfLeaves: Gen[Random with Sized, List[Leaf[String]]] =
    Gen.int(1, 20).flatMap(n => Gen.listOfN(n)(genLeaf))

  final case class Leaves[V](list: List[List[Leaf[V]]]) {
    def sequenceInner: List[Leaf[List[V]]] =
      list.map({ l =>
        Leaf((l.foldRight(List.empty[V]) { (a, acc) =>
          a.value :: acc
        }))
      })

    def toSequences[K]: List[Sequence[K, V]] =
      list.map(Sequence(_))

  }

  object Leaves {
    val genLeaves: Gen[Random with Sized, Leaves[String]] =
      Gen.int(1, 20).flatMap(n => Gen.listOfN(n)(genListOfLeaves)).map(Leaves(_))
  }

  private[config] val generate4Level: Gen[Random with Sized, (PropertyTree[String, String], Leaves[String])] =
    Leaves.genLeaves.map(l => {
      (
        Sequence(
          List(
            Sequence(
              List(
                Sequence(
                  l.toSequences
                )
              )
            )
          )
        ),
        l
      )
    })

  private[config] def generate3Level(l: List[Leaf[List[String]]]): PropertyTree[String, List[String]] =
    Sequence(
      List(
        Sequence(
          List(
            Sequence(
              l
            )
          )
        )
      )
    )
}
