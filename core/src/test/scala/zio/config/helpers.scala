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

  def genNumber(min: Int, max: Int): Gen[Random, Int] =
    for {
      n <- Gen.int(min, max)
    } yield n

  def genNonEmptyString(length: Int): Gen[Random, String] = genSymbol(1, length)

  def genDuration(length: Int): Gen[Random, String] = genNumber(1, length).map(days => s"$days day")

  val genId: Gen[Random, Id] = genSymbol(1, 5).map(Id)

  val genDbUrl: Gen[Random, DbUrl] = genNonEmptyString(20).map(DbUrl)

  def isErrors[A](assertion: Assertion[ReadError[String]]): Assertion[Either[ReadError[String], A]] =
    Assertion.assertionRec("isErrors")(Assertion.Render.param(assertion))(assertion) {
      case Left(errs: ReadError[String]) => Some(errs)
      case Right(_)                      => None
    }

  def assertErrors[A](
    pred: ReadError[String] => Boolean
  ): Assertion[Either[ReadError[String], A]] =
    assertion[Either[ReadError[String], A]]("assertErrors")() {
      case Left(errs: ReadError[String]) => pred(errs)
      case Right(_)                      => false
    }

  def toMultiMap[K, V](map: Map[K, V]): Map[K, ::[V]] =
    map.toList.map { case (k, v) => (k, singleton(v)) }.toMap

  def fromMultiMap[K, V](map: Map[K, ::[V]], appendString: String = ","): Map[K, String] =
    map.toList.map { case (k, v) => (k, v.mkString(appendString)) }.toMap

}
