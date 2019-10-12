package zio.config

import zio.config.helpers._
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
