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
          check(Gen.anyString)(assertValidRoundtrip(StringType, identity))
        },
        suite("BooleanType")(
          test("valid Boolean string roundtrip") {
            validBooleanStrings // proof
              .map(assertValidRoundtrip(BooleanType, _.toBoolean))
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
          genValid = genPadLeadingZeros(Gen.anyByte),
          invalidStringPredicate = _.toByteOption.isEmpty,
          parse = _.toByte
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Short",
          propType = ShortType,
          genValid = genPadLeadingZeros(Gen.anyShort),
          invalidStringPredicate = _.toShortOption.isEmpty,
          parse = _.toShort
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Int",
          propType = IntType,
          genValid = genPadLeadingZeros(Gen.anyInt),
          invalidStringPredicate = _.toIntOption.isEmpty,
          parse = _.toInt
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Long",
          propType = LongType,
          genValid = genPadLeadingZeros(Gen.anyLong),
          invalidStringPredicate = _.toLongOption.isEmpty,
          parse = _.toLong
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "BigInt",
          propType = BigIntType,
          genValid = genValidBigIntString,
          invalidStringPredicate = s => Try(BigInt(s)).isFailure,
          parse = BigInt(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Float",
          propType = FloatType,
          genValid = genPadLeadingZeros(Gen.anyFloat),
          invalidStringPredicate = _.toFloatOption.isEmpty,
          parse = _.toFloat
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Double",
          propType = DoubleType,
          genValid = genPadLeadingZeros(Gen.double(Double.MinValue, Double.MaxValue)),
          invalidStringPredicate = _.toDoubleOption.isEmpty,
          parse = _.toDouble
        ),
        // TODO: Uri, Duration
      )
    )

object PropertyTypeTestUtils {
  def propertyTypeRoundtripSuite[A](
    typeInfo: String,
    propType: PropertyType[String, A],
    genValid: Gen[Random with Sized, String],
    parse: String => A,
    invalidStringPredicate: String => Boolean
  ): Spec[TestEnvironment, TestFailure[Nothing], String, TestSuccess[Unit]] =
    suite(s"${typeInfo}Type")(
      testM(s"valid ${typeInfo} string roundtrip") {
        check(genValid.map(_.toString))(assertValidRoundtrip(propType, parse))
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
    parse: String => A
  )(s: String): TestResult =
    assert(roundTrip(propType, s).map(parse), isRight(equalTo(parse(s))))

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
    casePermutations("true") ++ casePermutations("false")
  val genInvalidBooleanString: Gen[Random with Sized, String] =
    genInvalidString(!validBooleanStrings.contains(_))

  val genValidBigIntString: Gen[Random with Sized, String] = for {
    sign <- Gen.oneOf(Gen.const(""), Gen.const("-"))
    num  <- Gen.listOf1(Gen.char('0', '9')).map(_.mkString)
  } yield sign + num

  def genPadLeadingZeros[A: Numeric](gen: Gen[Random, A]): Gen[Random with Sized, String] =
    (Gen.listOf(Gen.const('0')).map(_.mkString) <*> gen.map(_.toString)).map {
      case (zeros, num) if num.startsWith("-") => "-" + zeros + num.drop(1)
      case (zeros, num)                        => zeros + num
    }

  def genInvalidString(predicate: String => Boolean): Gen[Random with Sized, String] =
    Gen.anyString.filter(predicate)

  val leadingZeroes = "^0+(?!$)"
}
