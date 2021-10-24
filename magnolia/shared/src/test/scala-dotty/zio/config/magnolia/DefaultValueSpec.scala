package zio.config.magnolia

import zio.config.BaseSpec
import zio.{Has, Random}
import zio.test.Assertion.equalTo
import zio.test._
import DefaultValueSpecUtils._
import zio.config.magnolia.Macros

object DefaultValueSpec extends BaseSpec {
  val spec: Spec[Has[TestConfig] with Has[Random] with Has[Sized], TestFailure[Any], TestSuccess] =
    suite("magnolia spec")(
      test("default value for primitives") {
        assert(Macros.defaultValuesOf[A])(equalTo(List(("x", "defaultValue"))))
      },
      test("default value for nested types") {
        assert(Macros.defaultValuesOf[B])(equalTo(List(("y", A("nonDefaultValue")))))
      },

      test("default value for sealed trait types") {
        assert(Macros.defaultValuesOf[C])(equalTo(List(("z", X()))))
      },

      test("default value for case object") {
        assert(Macros.defaultValuesOf[D])(equalTo(List(("z", Z))))
      },

      test("default value for multiple values") {
        assert(Macros.defaultValuesOf[Mul])(equalTo(List(("a", A("x")), ("b", B(A("y"))), ("c", X()), ("d", Z))))
      },
    )
}

object DefaultValueSpecUtils {
  final case class A(x: String = "defaultValue")
  final case class B(y: A = A("nonDefaultValue"))

  sealed trait S

  case class X() extends S
  case class Y() extends S
  case object Z extends S

  final case class C(z: S = X())

  final case class D(z: S = Z)

  final case class Mul(a: A = A("x"), f: Int, b: B = B(A("y")), c: S = X(), d: Z.type = Z, e: String)
}
