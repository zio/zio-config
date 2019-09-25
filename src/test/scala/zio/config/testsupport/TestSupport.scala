package zio.config.testsupport

import org.scalacheck.{ Arbitrary, Gen }
import zio.{ DefaultRuntime, ZIO }

final class TestSupportOps[A](val actual: A) {
  def shouldBe(expected: A): Boolean = {
    val result = expected == actual
    if (!result) {
      println(s"       => FAIL: expected[$expected]")
      println(s"                  actual[$actual]")
    }
    result
  }

  def shouldSatisfy(f: A => Boolean): Boolean = {
    val result = f(actual)
    if (!result) {
      println(s"       => FAIL:   doesn't satisfy, actual: [$actual]")
    }
    result
  }
}

trait ToTestSupportOps {
  implicit def `instanceTestSupport`[A](actual: A): TestSupportOps[A] =
    new TestSupportOps[A](actual)
}

////

trait TestSupportGens {
  def genBoolean: Gen[Boolean] =
    Gen.posNum[Int].map(_ % 2 == 0)

  def genMultiline(genTestString: Gen[String]): Gen[(List[String], String)] =
    for {
      scount  <- Gen.chooseNum[Int](0, 20)
      strings <- Gen.listOfN(scount, genTestString)
    } yield strings -> strings.flatMap(s => List("\n", s, "\n")).mkString("\n")

  def genSymbol(minLength: Int, maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(minLength, maxLength)
      s <- Gen.listOfN(n, Gen.alphaChar).map(_.mkString)
    } yield s

  def genNonEmptyString(maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(1, maxLength)
      s <- Gen.listOfN(n, Gen.asciiChar).map(_.mkString)
    } yield s

  def genFor[A: Arbitrary]: Gen[A] =
    implicitly[Arbitrary[A]].arbitrary

}

////

final class TestSupportZIOOps[R, E, A](val io: ZIO[R, E, A]) extends AnyVal {
  def test(r: R): A =
    new DefaultRuntime {}
      .unsafeRun(io.provide(r))
}

trait ToTestSupportZIOOps {
  implicit def instanceTestSupportZIO[R, E, A](io: ZIO[R, E, A]): TestSupportZIOOps[R, E, A] =
    new TestSupportZIOOps(io)
}

////

final class TestSupportIOOps[E, A](val io: ZIO[Any, E, A]) extends AnyVal {
  def testResolved: A =
    new DefaultRuntime {}
      .unsafeRun(io)
}

trait ToTestSupportIOOps {
  implicit def instanceTestSupportIO[E, A](io: ZIO[Any, E, A]): TestSupportIOOps[E, A] =
    new TestSupportIOOps(io)
}

////

trait TestSupport extends ToTestSupportOps with TestSupportGens with ToTestSupportZIOOps with ToTestSupportIOOps

object testsupportinstances extends TestSupport
