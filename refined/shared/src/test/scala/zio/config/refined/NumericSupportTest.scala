package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{Divisible, Greater, GreaterEqual, Less, LessEqual, NonDivisible}
import zio.ZIO
import zio.config.{helpers, _}
import zio.test.Assertion._
import zio.test._

object NumericSupportTest extends BaseSpec {

  override val spec =
    suite("Refined Numeric support")(
      test("Refined config Less roundtrip") {
        check(Gen.int(1, 9).map(s => Refined.unsafeApply[Int, Less[W.`10`.T]](s))) { p =>
          val cfg = refine[Int, Less[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Less invalid") {
        check(Gen.int(10, 100)) { p =>
          val cfg                                                           = refine[Int, Less[W.`10`.T]]("TEST")
          val p2: ZIO[Any, ReadError[String], Refined[Int, Less[W.`10`.T]]] =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

          assertM(p2.either)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config Greater roundtrip") {
        check(Gen.int(11, 100).map(s => Refined.unsafeApply[Int, Greater[W.`10`.T]](s))) { p =>
          val cfg = refine[Int, Greater[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Greater invalid") {
        check(Gen.int(1, 10)) { p =>
          val cfg                                                              = refine[Int, Greater[W.`10`.T]]("TEST")
          val p2: ZIO[Any, ReadError[String], Refined[Int, Greater[W.`10`.T]]] =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString)))

          assertM(p2.either)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config LessEqual roundtrip") {
        check(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, LessEqual[W.`10`.T]](s))) { p =>
          val cfg = refine[Int, LessEqual[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config LessEqual invalid") {
        check(Gen.int(11, 100)) { p =>
          val cfg                                                                = refine[Int, LessEqual[W.`10`.T]]("TEST")
          val p2: ZIO[Any, ReadError[String], Refined[Int, LessEqual[W.`10`.T]]] =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

          assertM(p2.either)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config GreaterEqual roundtrip") {
        check(Gen.int(10, 100).map(s => Refined.unsafeApply[Int, GreaterEqual[W.`10`.T]](s))) { p =>
          val cfg = refine[Int, GreaterEqual[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config GreaterEqual invalid") {
        check(Gen.int(1, 9)) { p =>
          val cfg = refine[Int, GreaterEqual[W.`10`.T]]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      test("Refined config Divisible roundtrip") {
        check(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, Divisible[W.`10`.T]](s * 10))) { p =>
          val cfg = refine[Int, Divisible[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Divisible invalid") {
        check(Gen.int(1, 10).map(_ * 10 + 1)) { p =>
          val cfg = refine[Int, Divisible[W.`10`.T]]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      test("Refined config NonDivisible roundtrip") {
        check(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, NonDivisible[W.`10`.T]](s * 10 + 1))) { p =>
          val cfg = refine[Int, NonDivisible[W.`10`.T]]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "test"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config NonDivisible invalid") {
        check(Gen.int(1, 10).map(_ * 10)) { p =>
          val cfg = refine[Int, NonDivisible[W.`10`.T]]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      }
    )
}
