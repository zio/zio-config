package zio.config.testsupport

import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.{ Arbitrary, Gen, Prop, Shrink }
import zio.console.Console
import zio.{ DefaultRuntime, ZIO }

final class TestSupportZIOOps[E, A](val io: ZIO[Any, E, A]) {
  def shouldBe(expected: A): ZIO[Console, E, Boolean] =
    ZIO.accessM { env =>
      io.flatMap { actual =>
        val result = expected == actual
        if (!result) {
          env.console
            .putStrLn(
              s"""       => FAIL: expected[$expected]
                 |                  actual[$actual]""".stripMargin
            )
            .map(_ => result)
        } else {
          ZIO.succeed(result)
        }
      }
    }
  def shouldSatisfy(f: A => Boolean): ZIO[Console, E, Boolean] =
    ZIO.accessM { env =>
      io.flatMap { actual =>
        val result = f(actual)
        if (!result) {
          env.console
            .putStrLn(s"       => FAIL:   doesn't satisfy, actual: [$actual]")
            .map(_ => result)
        } else ZIO.succeed(result)
      }
    }
}

trait ToTestSupportZIOOps {
  implicit def `instanceTestSupportZIO`[E, A](io: ZIO[Any, E, A]): TestSupportZIOOps[E, A] =
    new TestSupportZIOOps[E, A](io)
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

trait TestSupportZIO extends DefaultRuntime {

  // ScalaCheck forAll except for ZIO
  def forAllZIO[T1, P, E](g1: Gen[T1])(f: T1 => ZIO[Environment, E, P])(
    implicit p: P => Prop,
    s1: Shrink[T1],
    pp1: T1 => Pretty
  ): Prop =
    forAll(g1) { t1 =>
      unsafeRun(f(t1))
    }

}

////

trait TestSupport extends ToTestSupportZIOOps with TestSupportGens with TestSupportZIO

object testsupportinstances extends TestSupport
