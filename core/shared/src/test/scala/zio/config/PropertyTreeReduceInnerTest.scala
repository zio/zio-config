package zio.config

import zio.test.Assertion._
import zio.test._

import PropertyTreeTestUtils._
import PropertyTree._

object PropertyTreeTest extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("PropertyTree.reduceInner")(
      test("reduceInner goes any n level deep tree and reduce the last Sequence(leaves) ") {
        check(nLevelSequenceWithLeaves) { input =>
          val (tree, leaves, levelCount) = input

          val reducedNLevel: PropertyTree[String, List[String]] =
            tree.map(_ :: Nil).reduceInner(_ ++ _)

          assert(reducedNLevel)(
            equalTo(generateAtleastNLevelSequenceTree(levelCount, leaves.sequenceInner))
          )
        }
      },
      test("reduceInner should convert a simple Sequence(List(Leaf(1))) to Leaf(List(1))") {
        check(genLeaf) { input =>
          assert(Sequence(List(input)).map(_ :: Nil).reduceInner(_ ++ _))(
            equalTo(Leaf(List(input.value)))
          )
        }
      },
      test(
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
      test(
        "reduceInner should keep the outer structure while reducing the inner sequence(leaves)"
      ) {
        check(genListOfLeaves) { input =>
          assert(Sequence(List(Sequence(input))).map(_ :: Nil).reduceInner(_ ++ _))(
            equalTo(Sequence(List(Leaf(input.map(_.value)))))
          )
        }
      },
      test("reduceInner should not change a simple leaf(list(v))") {
        check(genLeaf) { input =>
          assert(input.map(_ :: Nil).reduceInner(_ ++ _))(
            equalTo(input.map(_ :: Nil))
          )
        }
      }
    )
}
