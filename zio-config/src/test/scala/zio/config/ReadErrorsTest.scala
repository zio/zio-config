package zio.config

import org.scalacheck.Prop.forAll
import org.scalacheck.{ Gen, Properties }
import zio.config.ReadError.{ MissingValue, ParseError }
import zio.config.testsupport.TestSupport

object ReadErrorsTest extends Properties("ReadErrors/NEL") with TestSupport {

  private val genParseError =
    for {
      s1 <- genFor[String]
      s2 <- genFor[String]
    } yield ParseError(s1, s2)

  private val genReadError: Gen[ReadError] =
    Gen
      .oneOf(Gen.const(MissingValue), genParseError)
      .map(ReadError(List("somekey"), _))

  private val genReadErrors: Gen[List[ReadError]] =
    for {
      n    <- Gen.chooseNum(1, 20)
      list <- Gen.listOfN(n, genReadError)
    } yield list

  property("concat") = forAll(genReadErrors, genReadErrors) { (l1, l2) =>
    ReadErrors.concat(ReadErrors(l1.head, l1.tail: _*), ReadErrors(l2.head, l2.tail: _*)) ==
      l1 ++ l2
  }

}
