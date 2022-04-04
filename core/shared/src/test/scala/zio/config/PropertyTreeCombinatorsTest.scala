package zio.config

import zio.test.Assertion._
import zio.test.{Gen, _}

import PropertyTreeTestUtils._

object PropertyTreeCombinatorsTest extends BaseSpec {

  val spec =
    suite("PropertyTree.combinators")(
      test(
        "PropertyTree.map(tree, identity) returns tree"
      ) {
        check(nLevelSequenceWithRecords) { input =>
          val (tree, _, _) = input
          assert(tree.map(identity))(
            equalTo(tree)
          )
        }
      },
      test(
        "PropertyTree.map: follows functor associative law"
      ) {
        check(nLevelSequenceWithRecords, Gen.string, Gen.string) { (input, string, string2) =>
          val f: String => String = _ + string
          val g: String => String = _ + string2

          val (tree, _, _) = input

          val composed: String => String = g compose f
          assert(tree.map(f).map(g))(
            equalTo(tree.map(composed))
          )
        }
      },
      test(
        "PropertyTree.map(tree, f) ensures all leaves are mapped to f"
      ) {
        check(nLevelSequenceWithRecords, Gen.string) { (input, string) =>
          val (tree, leaves, params) = input
          assert(getTreeFromNLevelSequence(tree.map(_ + string), params.nestedSequencesCount))(
            equalTo((1 to params.recordKeyCount).toList.flatMap(_ => leaves.map(_ + string).toSequences))
          )
        }
      }
    )
}
