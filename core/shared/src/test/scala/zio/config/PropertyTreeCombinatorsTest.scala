package zio.config

import zio.test.Assertion._
import zio.test._

import PropertyTreeTestUtils._

object PropertyTreeCombinatorsTest extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("PropertyTree.combinators")(
      testM(
        "PropertyTree.map(tree, identity) returns tree"
      ) {
        check(nLevelSequenceWithRecords) { input =>
          val (tree, _, _) = input
          assert(tree.map(identity))(
            equalTo(tree)
          )
        }
      },
      testM(
        "PropertyTree.map: follows functor associative law"
      ) {
        check(nLevelSequenceWithRecords, Gen.anyString, Gen.anyString) { (input, string, string2) =>
          val f: String => String = _ + string
          val g: String => String = _ + string2

          val (tree, _, _) = input

          val composed: String => String = g compose f
          assert(tree.map(f).map(g))(
            equalTo(tree.map(composed))
          )
        }
      },
      testM(
        "PropertyTree.map(tree, f) ensures all leaves are mapped to f"
      ) {
        check(nLevelSequenceWithRecords, Gen.anyString) { (input, string) =>
          val (tree, leaves, params) = input
          assert(getTreeFromNLevelSequence(tree.map(_ + string), params.nestedSequencesCount))(
            equalTo((1 to params.recordKeyCount).toList.flatMap(_ => leaves.map(_ + string).toSequences))
          )
        }
      },
      testM(
        "PropertyTree.mapEmptyToError on a tree with zero empty returns all leaves with values in Right of Either"
      ) {
        check(nLevelSequenceWithRecords, Gen.anyString) { (input, string) =>
          val (tree, leaves, params) = input
          val mapEmpty               = tree.mapEmptyToError(string)
          assert(getTreeFromNLevelSequence(mapEmpty, params.nestedSequencesCount))(
            equalTo(
              (1 to params.recordKeyCount).toList
                .flatMap(_ => leaves.map(Right(_): Either[String, String]).toSequences)
            )
          )
        }
      },
      testM(
        "PropertyTree.mapEmptyToError on a tree with empty returns all leaves that are empty to Left(error)"
      ) {
        check(nLevelSequenceWithRecordsEmpty, Gen.anyString) { (input, string) =>
          val (tree, params) = input
          val mapEmpty       = tree.mapEmptyToError(string)

          assert(getTreeFromNLevelSequence(mapEmpty, params.nestedSequencesCount))(
            equalTo(
              (List
                .fill(params.recordKeyCount)(
                  PropertyTree.Leaf(Left(string))
                ))
            )
          )
        }
      },
      testM(
        "PropertyTree.zip should return the same tree on left and right when zipped with same tree"
      ) {
        check(nLevelSequenceWithRecords) { input =>
          val (tree, _, _) = input
          val zippedA      = tree.zipWith(tree)((a, _) => a)
          val zippedB      = tree.zipWith(tree)((_, b) => b)

          assert((zippedA, zippedB))(
            equalTo(
              ((tree, tree))
            )
          )
        }
      }
    )
}
