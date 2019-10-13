package zio.config

import zio.config.ReadErrors.ReadError
import zio.config.ReadErrorsTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object ReadErrorsTest
    extends BaseSpec(
      suite("ReadErrors/NEL")(
        testM("concat") {
          check(genReadErrors, genReadErrors) { (l1, l2) =>
            val actual = ReadErrors.concat(ReadErrors(l1.head, l1.tail: _*), ReadErrors(l2.head, l2.tail: _*)).errors
            assert(actual, equalTo(l1 ++ l2))
          }
        }
      )
    )

object ReadErrorsTestUtils {
  private val genParseError =
    for {
      s1 <- Gen.anyString
      s2 <- Gen.anyString
      s3 <- Gen.anyString
    } yield ReadError.ParseError(s1, s2, s3)

  private val genReadError =
    Gen.oneOf(Gen.const(ReadError.MissingValue("somekey")), genParseError)

  val genReadErrors: Gen[Random with Sized, List[ReadError[String, String]]] = {
    for {
      n    <- Gen.int(1, 20)
      list <- Gen.listOfN(n)(genReadError)
    } yield list
  }
}
