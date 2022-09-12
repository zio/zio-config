package zio.config

import zio.test.Assertion.assertion
import zio.test.{Assertion, ErrorMessage => M, Gen, TestArrow, TestTrace}

object helpers {
  final case class Id(value: String)
  final case class DbUrl(value: String)

  def genSymbol(min: Int, max: Int): Gen[Any, String] =
    for {
      n <- Gen.int(min, max)
      s <- Gen.listOfN(n)(Gen.alphaNumericChar)
    } yield s.mkString

  def genNumber(min: Int, max: Int): Gen[Any, Int] =
    for {
      n <- Gen.int(min, max)
    } yield n

  def genNonEmptyString(length: Int): Gen[Any, String] = genSymbol(1, length)

  def genDuration(length: Int): Gen[Any, String] = Gen.oneOf(
    genNumber(0, length).map(days => s"$days day"),
    genNumber(0, length).map(seconds => s"$seconds seconds"),
    genNumber(0, length).map(ms => s"$ms ms"),
    genNumber(0, length).map(ns => s"$ns ns")
  )

  val genId: Gen[Any, Id] = genSymbol(1, 5).map(Id.apply)

  val genDbUrl: Gen[Any, DbUrl] = genNonEmptyString(20).map(DbUrl.apply)

  def isErrors[A](assertion: Assertion[ReadError[String]]): Assertion[Either[ReadError[String], A]] =
    assertionRec("isErrors")(assertion) {
      case Left(errs: ReadError[String]) => Some(errs)
      case Right(_)                      => None
    }

  def assertErrors[A](
    pred: ReadError[String] => Boolean
  ): Assertion[Either[ReadError[String], A]] =
    assertion[Either[ReadError[String], A]]("assertErrors") {
      case Left(errs: ReadError[String]) => pred(errs)
      case Right(_)                      => false
    }

  def toMultiMap[K, V](map: Map[K, V]): Map[K, ::[V]]                                                           =
    map.toList.map { case (k, v) => (k, singleton(v)) }.toMap

  def fromMultiMap[K, V](map: Map[K, ::[V]], appendString: String = ","): Map[K, String]                        =
    map.toList.map { case (k, v) => (k, v.mkString(appendString)) }.toMap

  private def assertionRec[A, B](name: String)(assertion: Assertion[B])(get: (=> A) => Option[B]): Assertion[A] =
    Assertion(
      TestArrow
        .make[A, B] { a =>
          get(a).fold[TestTrace[B]](
            TestTrace.fail(M.text("Custom Assertion") + M.value(name) + M.choice("succeeded", "failed"))
          ) { b =>
            TestTrace.succeed(b)
          }
        }
        .withCode(name) >>> assertion.arrow
    )
}
