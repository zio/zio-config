package zio.config

import PropertyTypeTestUtils._
import zio.config.PropertyType._
import zio.random.Random
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

import scala.util.Try

object PropertyTypeTest
    extends BaseSpec(
      suite("Property type")(
        testM("StringType roundtrip") { // any string is a valid string?
          check(Gen.anyString)(assertValidRoundtrip(StringType, equalTo))
        },
        suite("BooleanType")(
          test("valid Boolean string roundtrip") {
            validTrueBooleans
              .map(assertValidRoundtrip(BooleanType, _ => equalTo("true")))
              .reduce(_ && _) &&
            validFalseBooleans
              .map(assertValidRoundtrip(BooleanType, _ => equalTo("false")))
              .reduce(_ && _)
          },
          testM("invalid Boolean string roundtrip") {
            check(genInvalidBooleanString)(
              assertInvalidRoundtrip(
                BooleanType,
                typeInfo = "Boolean",
                s => PropertyReadError(s, "boolean")
              )
            )
          }
        ),
        propertyTypeSuite(
          typeInfo = "Byte",
          ByteType,
          Gen.anyByte,
          _.toByteOption.isEmpty
        ),
        propertyTypeSuite(
          typeInfo = "Short",
          ShortType,
          Gen.anyShort,
          _.toShortOption.isEmpty
        ),
        propertyTypeSuite(
          typeInfo = "Int",
          IntType,
          Gen.anyInt,
          _.toIntOption.isEmpty
        ),
        propertyTypeSuite(
          typeInfo = "Long",
          LongType,
          Gen.anyLong,
          _.toLongOption.isEmpty
        ),
        suite("BigIntType")(
          testM("valid BigInt string roundtrip") {
            val leadingZeroes = "^0+(?!$)"
            check(genValidBigIntString)(
              assertValidRoundtrip(
                BigIntType,
                s => equalTo(s.replaceFirst(leadingZeroes, ""))
              )
            )
          },
          testM("invalid Bigint string roundtrip") {
            check(genInvalidBigIntString)(
              assertInvalidRoundtrip(
                BigIntType,
                typeInfo = "BigInt",
                s => PropertyReadError(s, "bigint")
              )
            )
          }
        ),
        propertyTypeSuite(
          typeInfo = "Float",
          FloatType,
          Gen.anyFloat,
          _.toFloatOption.isEmpty
        ),
        propertyTypeSuite(
          typeInfo = "Double",
          DoubleType,
          Gen.double(Double.MinValue, Double.MaxValue),
          _.toDoubleOption.isEmpty
        )
        // TODO: Add BigDecimal, Uri, Duration
      )
    )

object PropertyTypeTestUtils {
  def propertyTypeSuite[A](
    typeInfo: String,
    propType: PropertyType[String, A],
    genValid: Gen[Random, Any],
    invalidStringPredicate: String => Boolean,
  ): Spec[TestEnvironment, TestFailure[Nothing], String, TestSuccess[Unit]] =
    suite(s"${typeInfo}Type")(
      testM(s"valid ${typeInfo} string roundtrip") {
        check(genValid.map(_.toString))(assertValidRoundtrip(propType, equalTo))
      },
      testM(s"invalid ${typeInfo} string roundtrip") {
        check(genInvalidString(invalidStringPredicate))(
          assertInvalidRoundtrip(
            propType,
            typeInfo,
            s => PropertyReadError(s, typeInfo.toLowerCase)
          )
        )
      }
    )

  def assertValidRoundtrip[A](
    propType: PropertyType[String, A],
    stringAssertion: String => Assertion[String]
  )(s: String): TestResult =
    assert(roundTrip(propType, s), isRight(stringAssertion(s)))

  def assertInvalidRoundtrip[A](
    propType: PropertyType[String, A],
    typeInfo: String,
    propReadError: String => PropertyReadError[String]
  )(s: String): TestResult =
    assert(
      roundTrip(propType, s),
      isLeft(equalTo(propReadError(s)))
    )

  private def roundTrip[A](propType: PropertyType[String, A], s: String) =
    propType.read(s).map(propType.write)

  val validTrueBooleans: List[String]  = casePermutations("true")
  val validFalseBooleans: List[String] = casePermutations("false")

  /** Generates all case permutations of a string */
  private def casePermutations(s: String): List[String] = {
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

  val genValidBigIntString: Gen[Random with Sized, String] =
    Gen.listOf1(Gen.char('0', '9')).map(_.mkString)
  val genInvalidBigIntString =
    Gen.anyString.filter(s => Try(BigInt(s)).isFailure)

  def genInvalidString(predicate: String => Boolean): Gen[Random with Sized, String] =
    Gen.anyString.filter(predicate)
}
