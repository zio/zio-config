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
        }
      )
    )

object ReadErrorsTestUtils {
  private val genFormatError =
    for {
      s1 <- Gen.anyString
      s2 <- Gen.anyString
      s3 <- Gen.anyString
    } yield ReadError.FormatError(List(Step.Key(s1)), ReadFunctions.parseErrorMessage(s2, s3))

  private val genReadError =
    Gen.oneOf(Gen.const(ReadError.MissingValue(List(Step.Key("somekey")))), genFormatError)

  val genReadErrors: Gen[Random with Sized, List[ReadError]] = {
    for {
      n    <- Gen.int(1, 20)
      list <- Gen.listOfN(n)(genReadError)
    } yield list
  }
}
