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
      suite("PropertyType")(
        testM("StringType roundtrip") {
          // any string is a valid string i guess
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
        propertyTypeRoundtripSuite(
          typeInfo = "Byte",
          propType = ByteType,
          genValid = Gen.anyByte,
          invalidStringPredicate = _.toByteOption.isEmpty
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Short",
          propType = ShortType,
          genValid = Gen.anyShort,
          invalidStringPredicate = _.toShortOption.isEmpty
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Int",
          propType = IntType,
          genValid = Gen.anyInt,
          invalidStringPredicate = _.toIntOption.isEmpty
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Long",
          propType = LongType,
          genValid = Gen.anyLong,
          invalidStringPredicate = _.toLongOption.isEmpty
        ),
        suite("BigIntType")(
          testM("valid BigIntType string roundtrip") {
            check(genValidBigIntString.map(_.toString)) { s =>
              assert(BigIntType.read(s).map(BigIntType.write).map(BigInt(_)), isRight(equalTo(BigInt(s))))
            }
          },
          testM("invalid BigIntType string roundtrip") {
            check(genInvalidString(s => Try(BigInt(s)).isFailure))(
              assertInvalidRoundtrip(
                BigIntType,
                typeInfo = "BigInt",
                s => PropertyReadError(s, "bigint")
              )
            )
          }
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Float",
          propType = FloatType,
          genValid = Gen.anyFloat,
          invalidStringPredicate = _.toFloatOption.isEmpty
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Double",
          propType = DoubleType,
          genValid = Gen.double(Double.MinValue, Double.MaxValue),
          invalidStringPredicate = _.toDoubleOption.isEmpty
        ),
        // TODO: Uri, Duration
      )
    )

object PropertyTypeTestUtils {
  def propertyTypeRoundtripSuite[A](
    typeInfo: String,
    propType: PropertyType[String, A],
    genValid: Gen[Random with Sized, Any],
    invalidStringPredicate: String => Boolean,
    validStringPredicate: String => Assertion[String] = equalTo(_)
  ): Spec[TestEnvironment, TestFailure[Nothing], String, TestSuccess[Unit]] =
    suite(s"${typeInfo}Type")(
      testM(s"valid ${typeInfo} string roundtrip") {
        check(genValid.map(_.toString))(assertValidRoundtrip(propType, validStringPredicate))
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

  val genValidBigIntString: Gen[Random with Sized, String] = for {
    sign <- Gen.oneOf(Gen.const(""), Gen.const("-"))
    num  <- Gen.listOf1(Gen.char('0', '9')).map(_.mkString)
  } yield sign + num

  val genInvalidBigIntString: Gen[Random with Sized, String] =
    Gen.anyString.filter(s => Try(BigInt(s)).isFailure)

  def genInvalidString(predicate: String => Boolean): Gen[Random with Sized, String] =
    Gen.anyString.filter(predicate)

  val leadingZeroes = "^0+(?!$)"
}
