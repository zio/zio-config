package zio.config

import PropertyTypeTestUtils._
import zio.config.PropertyType._
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

object PropertyTypeTest
    extends BaseSpec(
      suite("Property type")(
        testM("StringType roundtrip") {
          check(Gen.anyString)(assertValidRoundtrip(StringType))
        },
        suite("BooleanType")(
          test("valid Boolean string roundtrip") {
            validTrueBooleans.map { s =>
              assert(
                BooleanType.read(s).map(BooleanType.write),
                isRight(equalTo("true"))
              )
            }.reduce(_ && _) && validFalseBooleans.map { s =>
              assert(
                BooleanType.read(s).map(BooleanType.write),
                isRight(equalTo("false"))
              )
            }.reduce(_ && _)
          },
          testM("invalid Boolean string roundtrip") {
            check(genInvalidBooleanString)(assertInvalidRoundtrip(BooleanType, typeInfo = "boolean"))
          }
        ),
        propertyTypeSpec(
          typeInfo = "Byte",
          ByteType,
          Gen.anyByte,
          _.toByteOption.isEmpty
        ),
        propertyTypeSpec(
          typeInfo = "Short",
          ShortType,
          Gen.anyShort,
          _.toShortOption.isEmpty
        ),
        propertyTypeSpec(
          typeInfo = "Int",
          IntType,
          Gen.anyInt,
          _.toIntOption.isEmpty
        ),
        propertyTypeSpec(
          typeInfo = "Long",
          LongType,
          Gen.anyLong,
          _.toLongOption.isEmpty
        ),
        // TODO: Add BigInt
        propertyTypeSpec(
          typeInfo = "Float",
          FloatType,
          Gen.anyFloat,
          _.toFloatOption.isEmpty
        ),
        propertyTypeSpec(
          typeInfo = "Double",
          DoubleType,
          Gen.double(Double.MinValue, Double.MaxValue),
          _.toDoubleOption.isEmpty
        )
        // TODO: Add BigDecimal, Uri, Duration
      )
    )

object PropertyTypeTestUtils {
  def propertyTypeSpec[A](
    typeInfo: String,
    propType: PropertyType[String, A],
    genValid: Gen[Random, Any],
    invalidStringPredicate: String => Boolean,
  ): Spec[TestEnvironment, TestFailure[Nothing], String, TestSuccess[Unit]] =
    suite(s"${typeInfo}Type")(
      testM(s"valid ${typeInfo} string roundtrip") {
        check(genValid.map(_.toString))(assertValidRoundtrip(propType))
      },
      testM(s"invalid ${typeInfo} string roundtrip") {
        check(genInvalidString(invalidStringPredicate))(assertInvalidRoundtrip(propType, typeInfo.toLowerCase))
      }
    )

  def assertValidRoundtrip[A](propType: PropertyType[String, A])(s: String): TestResult =
    assert(roundTrip(propType, s), isRight(equalTo(s)))

  def assertInvalidRoundtrip[A](propType: PropertyType[String, A], typeInfo: String)(s: String): TestResult =
    assert(roundTrip(propType, s), isLeft(equalTo(PropertyReadError(s, typeInfo))))

  private def roundTrip[A](propType: PropertyType[String, A], s: String) =
    propType.read(s).map(propType.write)

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
    genInvalidString(!validBooleanStrings.contains(_))

  val genInvalidByteString: Gen[Random with Sized, String] =
    genInvalidString(_.toByteOption.isEmpty)

  def genInvalidString(predicate: String => Boolean): Gen[Random with Sized, String] =
    Gen.anyString.filter(predicate)
}
