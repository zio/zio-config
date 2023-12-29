package zio.config.refined

import eu.timepit.refined.api.Refined
import zio.config._
import zio.test.Assertion._
import zio.test._
import zio.{Config, ConfigProvider, Scope, ZIO}

object NumericSupportTest extends ZIOSpecDefault with RequiredNumericTypes with NumericTestTypes {

  override val spec: Spec[Environment with TestEnvironment with Scope, Any] =
    suite("Refined Numeric support")(
      test("Refined config Less invalid") {
        check(Gen.int(10, 100)) { p =>
          val cfg                                              = refine[Int, Less10]("TEST")
          val p2: ZIO[Any, Config.Error, Refined[Int, Less10]] =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString), "test"))

          assertZIO(p2.either)(isLeft)
        }
      },
      test("Refined config Greater invalid") {
        check(Gen.int(1, 10)) { p =>
          val cfg                                                 = refine[Int, Greater10]("TEST")
          val p2: ZIO[Any, Config.Error, Refined[Int, Greater10]] =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString)))

          assertZIO(p2.either)(isLeft)
        }
      },
      test("Refined config LessEqual invalid") {
        check(Gen.int(11, 100)) { p =>
          val cfg                                                     = refine[Int, LessOrEqual10]("TEST")
          val p2: ZIO[Any, Config.Error, Refined[Int, LessOrEqual10]] =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString), "test"))

          assertZIO(p2.either)(isLeft)
        }
      },
      test("Refined config GreaterEqual invalid") {
        check(Gen.int(1, 9)) { p =>
          val cfg = refine[Int, GreaterOrEqual10]("TEST")
          val p2  =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString)))

          assertZIO(p2.either)(isLeft)
        }
      },
      test("Refined config Divisible invalid") {
        check(Gen.int(1, 10).map(_ * 10 + 1)) { p =>
          val cfg = refine[Int, DivisibleBy10]("TEST")
          val p2  =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString), "test"))

          assertZIO(p2.either)(isLeft)
        }
      },
      test("Refined config NonDivisible invalid") {
        check(Gen.int(1, 10).map(_ * 10)) { p =>
          val cfg = refine[Int, NonDivisibleBy10]("TEST")
          val p2  =
            read(cfg from ConfigProvider.fromMap(Map("TEST" -> p.toString), "test"))

          assertZIO(p2.either)(isLeft)
        }
      }
    )
}
