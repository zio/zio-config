package zio.config.testsupport

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

trait TestSupport
  extends ToTestSupportOps

object testsupportinstances
  extends TestSupport
