package zio.config

import zio.config.ReadError.Step
import zio.config.ReadErrorsTestUtils._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object ReadErrorsTest
    extends BaseSpec(
      suite("ReadErrors/NEL")(
        testM("concat") {
          check(genReadErrors, genReadErrors) { (l1, l2) =>
            val actual =
              concat(::(l1.head, l1.tail), ::(l2.head, l2.tail))
            assert(actual)(equalTo(l1 ++ l2))
          }
        },
        testM("prettyPrint of complex ReadError full text") {
          check(Gen.const(complexErrorsForPrettyPrint), Gen.const(complexErrorsPrettyPrint)) { (error, prettyPrint) =>
            assert(error.prettyPrint())(equalTo(prettyPrint))
          }
        }
      )
    )

object ReadErrorsTestUtils {
  private val genFormatError =
    for {
      s1 <- Gen.anyString
      s2 <- Gen.anyString
      s3 <- Gen.anyString
    } yield ReadError.FormatError(List(Step.Key(s1)), parseErrorMessage(s2, s3))

  private val genReadError =
    Gen.oneOf(Gen.const(ReadError.MissingValue(List(Step.Key("somekey")))), genFormatError)

  val genReadErrors: Gen[Random with Sized, List[ReadError[String]]] = {
    for {
      n    <- Gen.int(1, 20)
      list <- Gen.listOfN(n)(genReadError)
    } yield list
  }

  val simpleMissingValue    = ReadError.MissingValue(List(Step.Key("k1"), Step.Key("k2"), Step.Index(1), Step.Key("k3")))
  val simpleFormatError     = ReadError.FormatError(List(Step.Key("k1"), Step.Key("k2")), "Format error")
  val simpleConversionError = ReadError.ConversionError(List(Step.Key("k1"), Step.Key("k2")), "Conversion error")
  val complexErrorsForPrettyPrint = ReadError.ZipErrors(
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

  val complexErrorsPrettyPrint =
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
