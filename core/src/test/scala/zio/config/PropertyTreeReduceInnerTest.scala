package zio.config

import PropertyTreeTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import PropertyTree._

object PropertyTreeTest
    extends BaseSpec(
      suite("PropertyTree.reduceInner")(
        testM("reduceInner goes any n level deep tree and reduce the last Sequence(leaves) ") {
          check(nLevelSequenceWithLeaves) { input =>
            val (tree, leaves, n) = input

            val reducedNLevel: PropertyTree[String, List[String]] =
              tree.map(_ :: Nil).reduceInner(Nil)(_ ++ _)

            assert(
              reducedNLevel,
              equalTo(generateNLevelSequences(n, leaves.sequenceInner))
            )
          }
        },
        testM("reduceInner should convert a simple Sequence(List(Leaf(1))) to Leaf(List(1))") {
          check(genLeaf) { input =>
            assert(
              Sequence(List(input)).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Leaf(List(input.value)))
            )
          }
        },
        testM(
          "reduceInner should convert multiple leaves to a single leaves of multiple values"
        ) {
          check(genListOfLeaves) { input =>
            assert(
              Sequence(input).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Leaf(input.map(_.value)))
            )
          }
        },
        testM(
          "reduceInner should keep the outer structure while reducing the inner sequence(leaves)"
        ) {
          check(genListOfLeaves) { input =>
            assert(
              Sequence(List(Sequence(input))).map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Sequence(List(Leaf(input.map(_.value)))))
            )
          }
        },
        testM("reduceInner should not change a simple leaf(list(v))") {
          check(genLeaf) { input =>
            assert(
              input.map(_ :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(input.map(_ :: Nil))
            )
          }
        },
        testM("reduceInner should keep the empty nodes as it is") {
          check(Gen.int(1, 20)) { input =>
            val listOfEmpty = List.fill(input)(PropertyTree.empty)
            assert(
              Sequence(listOfEmpty).map((a: Any) => a :: Nil).reduceInner(Nil)(_ ++ _),
              equalTo(Sequence(listOfEmpty).map((a: Any) => a :: Nil))
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
      Gen.int(1, 3).flatMap(n => Gen.listOfN(n)(genListOfLeaves)).map(Leaves(_))
  }

  private[config] val nLevelSequenceWithLeaves
    : Gen[Random with Sized, (PropertyTree[String, String], Leaves[String], Int)] =
    Leaves.genLeaves.flatMap(
      l =>
        Gen
          .int(1, 3)
          .map(
            n => (generateNLevelSequences(n, l.toSequences), l, n)
          )
    )

  private[config] def generateNLevelSequences[A](
    n: Int,
    inject: List[PropertyTree[String, A]]
  ): PropertyTree[String, A] =
    if (n <= 1) {
      Sequence(inject)

    } else {
      Sequence(List(generateNLevelSequences(n - 1, inject)))
    }

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
