package zio.config

import zio.config.PropertyTreePath.Step
import zio.config.ReadErrorsTestUtils._
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test.{Gen, Sized, _}

object ReadErrorsTest extends BaseSpec {

  val spec: Spec[Annotations with TestConfig with Sized, Nothing] =
    suite("ReadErrors/NEL")(
      test("concat") {
        check(genReadErrors, genReadErrors) { (l1, l2) =>
          val actual =
            concat(::(l1.head, l1.tail), ::(l2.head, l2.tail))
          assert(actual)(equalTo(l1 ++ l2))
        }
      },
      test("prettyPrint of complex ReadError full text") {
        val error       = complexErrorsForPrettyPrint
        val prettyPrint = complexErrorsPrettyPrint

        assertTrue(error.prettyPrint() == prettyPrint)
      } @@ ignore
    )
}

object ReadErrorsTestUtils {
  private val genFormatError =
    for {
      s1 <- Gen.string
      s2 <- Gen.string
      s3 <- Gen.string
    } yield ReadError.FormatError(List(Step.Key(s1)), parseErrorMessage(s2, s3))

  private val genReadError: Gen[Sized, Config.Error] =
    Gen.oneOf(Gen.const(ReadError.MissingValue(List(Step.Key("somekey")))), genFormatError)

  val genReadErrors: Gen[Sized, List[Config.Error]] = {
    for {
      n    <- Gen.int(1, 20)
      list <- Gen.listOfN(n)(genReadError)
    } yield list
  }

  val simpleMissingValue: ReadError.MissingValue[String]       =
    ReadError.MissingValue(List(Step.Key("k1"), Step.Key("k2"), Step.Index(1), Step.Key("k3")))
  val simpleFormatError: ReadError.FormatError[String]         =
    ReadError.FormatError(List(Step.Key("k1"), Step.Key("k2")), "Format error")
  val simpleConversionError: ReadError.ConversionError[String] =
    ReadError.ConversionError(List(Step.Key("k1"), Step.Key("k2")), "Conversion error")
  val complexErrorsForPrettyPrint: ReadError.ZipErrors[String] = ReadError.ZipErrors(
    List(
      simpleMissingValue,
      simpleFormatError,
      simpleConversionError,
      ReadError.OrErrors(
        List(
          ReadError.ZipErrors(
            List(ReadError.OrErrors(List(simpleMissingValue, simpleMissingValue)), simpleFormatError)
          ),
          ReadError.OrErrors(List(simpleMissingValue, simpleMissingValue)),
          simpleConversionError
        )
      ),
      ReadError.ZipErrors(List(ReadError.OrErrors(List(simpleMissingValue, simpleMissingValue)), simpleFormatError))
    )
  )

  val complexErrorsPrettyPrint: String =
    """ReadError:
      |╥
      |╠══╦══╦══╦══╦══╦══╗
      |║  ║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ║  ╠─FormatError
      |║  ║  ║  ║  ║  ║  ║ cause: Format error
      |║  ║  ║  ║  ║  ║  ║ path: k1.k2
      |║  ║  ║  ║  ║  ║  ▼
      |║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║  ▼
      |║  ║  ║  ║  ║
      |║  ║  ║  ║  ╠══╦══╗
      |║  ║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ║  ╠─FormatError
      |║  ║  ║  ║  ║  ║  ║ cause: Format error
      |║  ║  ║  ║  ║  ║  ║ path: k1.k2
      |║  ║  ║  ║  ║  ║  ▼
      |║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║  ║
      |║  ║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║  ▼
      |║  ║  ║  ║  ║
      |║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║
      |║  ║  ║  ║  ╠─MissingValue
      |║  ║  ║  ║  ║ path: k1.k2[1].k3
      |║  ║  ║  ║  ║
      |║  ║  ║  ║  ╠─ConversionError
      |║  ║  ║  ║  ║ cause: Conversion error
      |║  ║  ║  ║  ║ path: k1.k2
      |║  ║  ║  ║  ▼
      |║  ║  ║  ║
      |║  ║  ║  ╠─ConversionError
      |║  ║  ║  ║ cause: Conversion error
      |║  ║  ║  ║ path: k1.k2
      |║  ║  ║  ▼
      |║  ║  ║
      |║  ║  ╠─FormatError
      |║  ║  ║ cause: Format error
      |║  ║  ║ path: k1.k2
      |║  ║  ▼
      |║  ║
      |║  ╠─MissingValue
      |║  ║ path: k1.k2[1].k3
      |║  ▼
      |▼""".stripMargin
}
