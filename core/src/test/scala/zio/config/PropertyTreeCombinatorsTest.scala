package zio.config

import PropertyTreeTestUtils._
import zio.test._
import zio.test.Assertion._

object PropertyTreeCombinatorsTest
    extends BaseSpec(
      suite("PropertyTree.map")(
        testM(
          "When tree mapped with identity gives the same tree"
        ) {
          check(nLevelSequenceWithRecords) { input =>
            {
              val (tree, _, _) = input
              assert(
                tree.map(identity),
                equalTo(tree)
              )
            }
          }
        },
        testM(
          "PropertyTree.map follows functor associative law"
        ) {
          check(nLevelSequenceWithRecords, Gen.anyString, Gen.anyString) { (input, string, string2) =>
            {
              val f: String => String = _ + string
              val g: String => String = _ + string2

              val (tree, _, _) = input

              val composed: String => String = g compose f
              assert(
                tree.map(f).map(g),
                equalTo(tree.map(composed))
              )
            }
          }
        },
        testM(
          "When tree mapped with any f change all the leaf values of both records and sequences"
        ) {
          check(nLevelSequenceWithRecords, Gen.anyString) { (input, string) =>
            {
              val (tree, leaves, params) = input
              assert(
                getTreeFromNLevelSequence(tree.map(_ + string), params.nestedSequencesCount),
                equalTo((1 to params.numberOfKeysInRecordMap).toList.flatMap(_ => leaves.map(_ + string).toSequences))
              )
            }
          }
        }
      )
    )
