package zio.config

import ReadErrorsTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

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
    } yield ReadError.FormatError(Vector(Right(s1)), ReadFunctions.parseErrorMessage(s2, s3))

  private val genReadError =
    Gen.oneOf(Gen.const(ReadError.MissingValue(Vector(Right("somekey")))), genFormatError)

  val genReadErrors: Gen[Random with Sized, List[ReadError[String]]] = {
    for {
      n    <- Gen.int(1, 20)
      list <- Gen.listOfN(n)(genReadError)
    } yield list
  }
}
