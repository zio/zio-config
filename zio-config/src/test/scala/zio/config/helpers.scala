package zio.config

import zio.random.Random
import zio.test.Assertion.assertion
import zio.test.{ Assertion, Gen }

object helpers {
  final case class Id(value: String)    extends AnyVal
  final case class DbUrl(value: String) extends AnyVal

  def genSymbol(min: Int, max: Int): Gen[Random, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.listOfN(n)(Gen.alphaNumericChar)
    } yield s.mkString

  def genNonEmptyString(length: Int): Gen[Random, String] = genSymbol(1, length)

  val genId: Gen[Random, Id] = genSymbol(1, 5).map(Id)

  val genDbUrl: Gen[Random, DbUrl] = genNonEmptyString(20).map(DbUrl)

  def assertErrors[A](
    pred: ReadErrorsVector[String, String] => Boolean
  ): Assertion[Either[ReadErrorsVector[String, String], A]] =
    assertion[Either[ReadErrorsVector[String, String], A]]("assertErrors")() {
      case Left(errs: ReadErrorsVector[String, String]) => pred(errs)
      case Right(_)                                     => false
    }

}
