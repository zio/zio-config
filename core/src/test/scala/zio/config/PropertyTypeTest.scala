package zio.config

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset }
import java.util.UUID

import zio.config.PropertyType._
import zio.config.PropertyTypeTestUtils._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import scala.concurrent.duration.Duration
import scala.util.Try

object PropertyTypeTest
    extends BaseSpec(
      suite("PropertyType")(
        testM("StringType roundtrip") {
          // any string is a valid string i guess
          check(Gen.anyString)(
            assertValidRoundtrip(StringType, parse = identity)
          )
        },
        propertyTypeRoundtripSuite(
          typeInfo = "Boolean",
          propType = BooleanType,
          genValid = genValidBooleanStrings,
          parse = input =>
            input.toLowerCase match {
              case "true" | "on" | "1"   => true
              case "false" | "off" | "0" => false
              case _                     => throw new IllegalArgumentException(s"For input string: '$input'")
            }
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Byte",
          propType = ByteType,
          genValid = genPadLeadingZeros(Gen.anyByte),
          parse = _.toByte
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Short",
          propType = ShortType,
          genValid = genPadLeadingZeros(Gen.anyShort),
          parse = _.toShort
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Int",
          propType = IntType,
          genValid = genPadLeadingZeros(Gen.anyInt),
          parse = _.toInt
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Long",
          propType = LongType,
          genValid = genPadLeadingZeros(Gen.anyLong),
          parse = _.toLong
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "BigInt",
          propType = BigIntType,
          genValid = genValidBigIntString,
          parse = BigInt(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Float",
          propType = FloatType,
          genValid = genPadLeadingZeros(Gen.anyFloat),
          parse = _.toFloat
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Double",
          propType = DoubleType,
          genValid = genPadLeadingZeros(Gen.double(Double.MinValue, Double.MaxValue)),
          parse = _.toDouble
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "BigDecimal",
          propType = BigDecimalType,
          genValid = genValidBigDecimalString,
          parse = BigDecimal(_)
        ),
        testM(s"valid URI string roundtrip") {
          check(Gen.anyString)(assertValidRoundtrip(UriType, new URI(_)))
        },
        propertyTypeRoundtripSuite(
          typeInfo = "Duration",
          propType = DurationType,
          genValid = Gen.sized(helpers.genDuration),
          parse = Duration(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "UUID",
          propType = UuidType,
          genValid = Gen.anyUUID.map(_.toString),
          parse = UUID.fromString(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Duration",
          propType = ZioDurationType,
          genValid = Gen.sized(helpers.genDuration),
          parse = s => zio.duration.Duration.fromScala(Duration(s))
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "LocalDate",
          propType = LocalDateType,
          genValid = genLocalDateString,
          parse = LocalDate.parse(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "LocalDateTime",
          propType = LocalDateTimeType,
          genValid = genLocalDateTimeString,
          parse = LocalDateTime.parse(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "LocalTime",
          propType = LocalTimeType,
          genValid = genLocalTimeString,
          parse = LocalTime.parse(_)
        ),
        propertyTypeRoundtripSuite(
          typeInfo = "Instant",
          propType = InstantType,
          genValid = genInstant.map(_.toString),
          parse = Instant.parse(_)
        ),
        testM(s"valid FileType string roundtrip") {
          check(Gen.anyString)(assertValidRoundtrip(FileType, new File(_)))
        },
        propertyTypeRoundtripSuite(
          typeInfo = "URL",
          propType = UrlType,
          genValid = genValidUrlString,
          parse = new URL(_)
        ),
        testM("valid JavaFilePathType string roundtrip") {
          check(Gen.anyString)(assertValidRoundtrip(JavaFilePathType, java.nio.file.Paths.get(_)))
        }
      )
    )

object PropertyTypeTestUtils {

  def propertyTypeRoundtripSuite[A](
    typeInfo: String,
    propType: PropertyType[String, A],
    genValid: Gen[Random with Sized, String],
    parse: String => A
  ): Spec[TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite(s"${typeInfo}Type")(
      testM(s"valid ${typeInfo} string roundtrip") {
        check(genValid.map(_.toString))(assertValidRoundtrip(propType, parse))
      },
      testM(s"invalid ${typeInfo} string roundtrip") {
        val invalidString = Gen.anyString.filter(s => Try(parse(s)).isFailure)
        check(invalidString)(
          assertInvalidRoundtrip(
            propType,
            s => PropertyReadError(s, typeInfo.toLowerCase)
          )
        )
      }
    )

  def assertValidRoundtrip[A](
    propType: PropertyType[String, A],
    parse: String => A
  )(s: String): TestResult =
    assert(roundTrip(propType, s).map(parse))(isRight(equalTo(parse(s))))

  def assertInvalidRoundtrip[A](
    propType: PropertyType[String, A],
    propReadError: String => PropertyReadError[String]
  )(s: String): TestResult =
    assert(roundTrip(propType, s))(isLeft(equalTo(propReadError(s))))

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

  import Gen._

  val validBooleanStrings: List[String] =
    casePermutations("true") ++ casePermutations("false") ++ casePermutations("on") ++ casePermutations("off") ++ List(
      "1",
      "0"
    )

  val genValidBooleanStrings: Gen[Any, String] = Gen.fromIterable(validBooleanStrings)

  private def genOptionalStr(gen: Gen[Random with Sized, String]) =
    oneOf(const(""), gen)

  private val genDigit0To9: Gen[Random, String] = char('0', '9').map(_.toString)
  private val genDigit1To9: Gen[Random, String] = char('1', '9').map(_.toString)

  /**
   * Generates a string of up to n digits. Default maxLength digits is 100.
   */
  private def genDigits(maxLength: Int = 100): Gen[Random with Sized, String] =
    sized(n => listOfN(math.min(n, maxLength))(genDigit0To9).map(_.mkString))

  val genValidBigIntString: Gen[Random with Sized, String] = for {
    sign   <- genOptionalStr(const("-"))
    digits <- genDigits()
  } yield sign + digits

  // Should be generalized and written in terms of `sequence`, or `traverse`
  private def genAppend(
    as: Gen[Random with Sized, String]*
  ): Gen[Random with Sized, String] =
    for {
      str  <- as.headOption.fold[Gen[Random with Sized, String]](const(""))(identity)
      rest <- if (as.isEmpty) const("") else genAppend(as.tail: _*)
    } yield str + rest

  private val leadingZeros: Gen[Random with Sized, String] =
    listOf(const('0')).map(_.mkString)

  def genPadLeadingZeros[A: Numeric](gen: Gen[Random, A]): Gen[Random with Sized, String] =
    (leadingZeros <*> gen.map(_.toString)).map {
      case (zeros, num) =>
        if (num.startsWith("-")) "-" + zeros + num.drop(1)
        else zeros + num
    }

  private val genWhole: Gen[Random with Sized, String] = oneOf(
    const("0"),
    genAppend(genDigit1To9, genDigits())
  )

  private val genDecimal: Gen[Random with Sized, String] = oneOf(
    genOptionalStr(const(".")),
    genAppend(const("."), genDigits())
  )

  private val genExponent: Gen[Random with Sized, String] = genAppend(
    oneOf(const("e"), const("E")),
    genOptionalStr(oneOf(const("+"), const("-"))),
    genAppend(genDigit0To9, genDigit0To9) // let's keep it reasonable...
  )

  // loose reference https://i.stack.imgur.com/wmFqa.gif
  val genValidBigDecimalString: Gen[Random with Sized, String] = genAppend(
    genOptionalStr(const("-")),
    genAppend(leadingZeros, genWhole),
    genOptionalStr(genDecimal),
    genOptionalStr(genExponent)
  )

  val genAlphaChar: Gen[Random, Char] =
    oneOf(char(65, 90), char(97, 122))

  private val genScheme: Gen[Random with Sized, String] = for {
    letter <- genAlphaChar.map(_.toString)
    rest <- listOf(
             weighted(
               alphaNumericString      -> 36,
               elements("+", ".", "-") -> 3
             )
           ).map(_.mkString)
  } yield letter + rest

  private val genAuth: Gen[Random with Sized, String] = genAppend(
    const("//"),
    genAppend(genOptionalStr(alphaNumericString), const("@")), // userinfo
    const("@"),
    Gen.weighted(alphaNumericString -> 26), // host
    genOptionalStr(genAppend(const(":"), int(0, 65535).map(_.toString))) // port
  )

  private val genPath: Gen[Random with Sized, String] =
    listOf1(listOf1(alphaNumericChar).map(_.mkString)).map(_.mkString("/"))

  private val genAuthorityAndPath: Gen[Random with Sized, String] = {
    for {
      hasAuthority <- boolean
      authAndPath <- if (hasAuthority) {
                      genAppend(genAuth, const("/"), genPath)
                    } else genPath
    } yield authAndPath
  }
  private val genQuery: Gen[Random with Sized, String] = genAppend(
    const("?"),
    oneOf(alphaNumericString, listOfN(2)(alphaNumericString).map(_.mkString("=")))
  )

  private val genFragment: Gen[Random with Sized, String] =
    alphaNumericString

  /**
   * Generates a valid URI string, i.e. one `java.net.Uri` will accept.
   *
   * Not complete, mind you.
   *
   * <br/><br/>
   * reference: https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Generic_syntax
   */
  val genValidUriString: Gen[Random with Sized, String] = genAppend(
    genScheme,
    const(":"),
    genAuthorityAndPath,
    genOptionalStr(genQuery),
    genOptionalStr(genFragment)
  )

  val genValidUrlString: Gen[Random with Sized, String] = genAppend(
    Gen.elements("http", "https", "ftp", "file"),
    const(":"),
    genAuthorityAndPath,
    genOptionalStr(genQuery),
    genOptionalStr(genFragment)
  )

  val genInstant: Gen[Random, Instant] =
    Gen.anyLong.map(Instant.ofEpochMilli)

  val genLocalDateString: Gen[Random with Sized, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalDate.toString)

  val genLocalDateTimeString: Gen[Random with Sized, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalDateTime.toString)

  val genLocalTimeString: Gen[Random with Sized, String] =
    genInstant.map(_.atZone(ZoneOffset.UTC).toLocalTime.toString)

}
