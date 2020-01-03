package zio.config

import PropertyTypeTestUtils._
import zio.config.PropertyType._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object PropertyTypeTest
    extends BaseSpec(
      suite("Property type")(
        testM("StringType roundtrip") {
          check(Gen.anyString) { s =>
            assert(StringType.read(s).map(StringType.write), isRight(equalTo(s)))
          }
        },
        suite("BooleanType")(
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
        ),
        suite("ByteType")(
          test("valid byte string roundtrip") {
            (Byte.MinValue to Byte.MaxValue).map { n =>
              val s = n.toString
              assert(ByteType.read(s).map(ByteType.write), isRight(equalTo(s)))
            }.reduce(_ && _)
          },
          testM("invalid byte string roundtrip") {
            check(genInvalidByteString) { s =>
              assert(
                ByteType.read(s).map(ByteType.write),
                isLeft(equalTo(PropertyReadError(s, "byte")))
              )
            }
          }
        ),
        suite("Short")(
          testM("valid short string roundtrip") {
            check(genValidShortStrings) { s =>
              assert(
                ShortType.read(s).map(ShortType.write),
                isRight(equalTo(s))
              )
            }
          }
        )
      )
    )

object PropertyTypeTestUtils {

  val validTrueBooleans: List[String]  = stringCasePermutations("true")
  val validFalseBooleans: List[String] = stringCasePermutations("false")

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

  val validBooleanStrings: List[String] =
    validTrueBooleans ++ validFalseBooleans
  val genInvalidBooleanString: Gen[Random with Sized, String] =
    genInvalidStrings(!validBooleanStrings.contains(_))

  val genInvalidByteString: Gen[Random with Sized, String] =
    genInvalidStrings(_.toByteOption.isEmpty)

  val genValidShortStrings: Gen[Random, String] =
    Gen.short(Short.MinValue, Short.MaxValue).map(_.toString)

  def genInvalidStrings(predicate: String => Boolean): Gen[Random with Sized, String] =
    Gen.anyString.filter(predicate)

}
