package zio.config

import PropertyTreeTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import PropertyTree._

object PropertyTreeTest
    extends BaseSpec(
      suite("PropertyTree.reduceInner")(
        testM("reduceInner converts a simple Sequence(List(Leaf(1))) to Leaf(List(1))") {
          check(genLeaf) { input =>
            assert(
              Sequence(List(input)).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Leaf(List(input.value)))
            )
          }
        },
        testM(
          "reduceInner converts multiple leaves to a single leaves of multiple values"
        ) {
          check(genListOfLeaves) { input =>
            assert(
              Sequence(input).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Leaf(input.map(_.value)))
            )
          }
        },
        testM(
          "reduceInner only inner Sequence of leaves keeping the outer structure "
        ) {
          check(genListOfLeaves) { input =>
            assert(
              Sequence(List(Sequence(input))).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Sequence(List(Leaf(input.map(_.value)))))
            )
          }
        },
        testM("reduceInner doesn't change a simple leaf(list(v)) further") {
          check(genLeaf) { input =>
            assert(
              input.map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(input.map(_ :: Nil))
            )
          }
        },
        testM("reduceInner keeps the empty nodes as it is") {
          check(Gen.int(1, 20)) { input =>
            val listOfEmpty = List.fill(input)(PropertyTree.empty)
            assert(
              Sequence(listOfEmpty).map((a: Any) => a :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Sequence(listOfEmpty).map((a: Any) => a :: Nil))
            )
          }
        },
        testM("reduceInner goes 4 level deep and sequence the Sequence(Leaves)") {
          check(generate4Level) { input =>
            val (tree, leaves) = input

            val reducedTo3Level: PropertyTree[String, List[String]] =
              tree.map(_ :: Nil).reduceInner(Nil)(_ ++ _)

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
