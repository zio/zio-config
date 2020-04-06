package zio.config

import PropertyTreeTestUtils._
import zio.test._
import zio.test.Assertion._
import PropertyTree._

object PropertyTreeTest
    extends BaseSpec(
      suite("PropertyTree.reduceInner")(
        testM("reduceInner goes any n level deep tree and reduce the last Sequence(leaves) ") {
          check(nLevelSequenceWithLeaves) { input =>
            val (tree, leaves, levelCount) = input

            val reducedNLevel: PropertyTree[String, List[String]] =
              tree.map(_ :: Nil).reduceInner(_ ++ _)

            assert(reducedNLevel)(
              equalTo(generateAtleastNLevelSequenceTree(levelCount, leaves.sequenceInner))
            )
          }
        },
        testM("reduceInner should convert a simple Sequence(List(Leaf(1))) to Leaf(List(1))") {
          check(genLeaf) { input =>
            assert(Sequence(List(input)).map(_ :: Nil).reduceInner(_ ++ _))(
              equalTo(Leaf(List(input.value)))
            )
          }
        },
        testM(
          "reduceInner should convert multiple leaves to a single leaf of multiple values"
        ) {
          check(genListOfLeaves) { input =>
            assert(Sequence(input).map(_ :: Nil).reduceInner(_ ++ _))(
              equalTo(Leaf(input.map(_.value)))
            )
          }
        },
        test("reduceInner should not reduce a Sequence(Nil) as it is unaware of the structure of Nil") {
          val input =
            Sequence(Nil: List[PropertyTree[Nothing, Nothing]]).map((_: Any) :: Nil)

          assert(input.reduceInner(_ ++ _))(
            equalTo(input)
          )
        },
        testM(
          "reduceInner should keep the outer structure while reducing the inner sequence(leaves)"
        ) {
          check(genListOfLeaves) { input =>
            assert(Sequence(List(Sequence(input))).map(_ :: Nil).reduceInner(_ ++ _))(
              equalTo(Sequence(List(Leaf(input.map(_.value)))))
            )
          }
        },
        testM("reduceInner should not change a simple leaf(list(v))") {
          check(genLeaf) { input =>
            assert(input.map(_ :: Nil).reduceInner(_ ++ _))(
              equalTo(input.map(_ :: Nil))
            )
          }
        }
        // testM("reduceInner should keep the empty nodes as it is") {
        //   check(Gen.int(1, 20)) { input =>
        //     val listOfEmpty = List.fill(input)(PropertyTree.empty)
        //     assert(Sequence(listOfEmpty).map((a: Any) => a :: Nil).reduceInner(_ ++ _))(
        //       equalTo(Sequence(listOfEmpty).map((a: Any) => a :: Nil))
        //     )
        //   }
        // }
      )
    )
