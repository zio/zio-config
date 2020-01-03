package zio.config

import PropertyTypeTestUtils._
import zio.config.PropertyType._
import zio.test._
import zio.test.Assertion._

object PropertyTypeTest
    extends BaseSpec(
      suite("Property type")(
        testM("String roundtrip") {
          check(Gen.anyString) { s =>
            assert(StringType.read(s).map(StringType.write), isRight(equalTo(s)))
          }
        },
        suite("Boolean")(
          test("valid boolean string roundtrip") {
            validTrueBooleans.map { s =>
              assert(BooleanType.read(s).map(BooleanType.write), isRight(equalTo("true")))
            }.reduce(_ && _) && validFalseBooleans.map { s =>
              assert(BooleanType.read(s).map(BooleanType.write), isRight(equalTo("false")))
            }.reduce(_ && _)
          },
          testM("invalid boolean string roundtrip") {
            check(genInvalidBooleanString) { s =>
              assert(
                BooleanType.read(s).map(BooleanType.write),
                isLeft(equalTo(PropertyReadError(s, "boolean")))
              )
            }
          }
        )
      )
    )

object PropertyTypeTestUtils {

  val validTrueBooleans: List[String]   = stringCasePermutations("true")
  val validFalseBooleans: List[String]  = stringCasePermutations("false")
  val validBooleanStrings: List[String] = validTrueBooleans ++ validFalseBooleans

  val genInvalidBooleanString =
    Gen.anyString.filter(!validBooleanStrings.contains(_))

  /** Generates all case permutations of a string */
  private def stringCasePermutations(s: String): List[String] = {
    val max          = 1 << s.length
    val s_           = s.toLowerCase
    val combinations = Array.ofDim[String](max)
    for (i <- 0 until max) {
      val combination: Array[Char] = s_.toCharArray
      for (j <- 0 until s_.length) {
        if (((i >> j) & 1) == 1)
          combination(j) = (combination(j) - 32).toChar
      }
      combinations(i) = combination.mkString
    }
    combinations.toList
  }

}
