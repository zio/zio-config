package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{ Divisible, Greater, GreaterEqual, Less, LessEqual, NonDivisible }
import zio.ZIO
import zio.config.{ helpers, BaseSpec, ConfigSource, ReadError }
import zio.test.Assertion._
import zio.config._, ConfigDescriptor._
import zio.test._

object NumericSupportTest
    extends BaseSpec(
      suite("Refined Numeric support")(
        testM("Refined config Less roundtrip") {
          checkM(Gen.int(1, 9).map(s => Refined.unsafeApply[Int, Less[W.`10`.T]](s))) { p =>
            val cfg = less[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Less invalid") {
          checkM(Gen.int(10, 100)) { p =>
            val cfg = less[W.`10`.T](int("TEST"))
            val p2: ZIO[Any, ReadError[String], Refined[Int, Less[W.`10`.T]]] =
              ZIO.fromEither(read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test")))

            assertM(p2.either)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config Greater roundtrip") {
          checkM(Gen.int(11, 100).map(s => Refined.unsafeApply[Int, Greater[W.`10`.T]](s))) { p =>
            val cfg = greater[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Greater invalid") {
          checkM(Gen.int(1, 10)) { p =>
            val cfg = greater[W.`10`.T](int("TEST"))
            val p2: ZIO[Any, ReadError[String], Refined[Int, Greater[W.`10`.T]]] =
              ZIO.fromEither(read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString))))

            assertM(p2.either)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config LessEqual roundtrip") {
          checkM(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, LessEqual[W.`10`.T]](s))) { p =>
            val cfg = lessEqual[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config LessEqual invalid") {
          checkM(Gen.int(11, 100)) { p =>
            val cfg = lessEqual[W.`10`.T](int("TEST"))
            val p2: ZIO[Any, ReadError[String], Refined[Int, LessEqual[W.`10`.T]]] =
              ZIO.fromEither(read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test")))

            assertM(p2.either)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config GreaterEqual roundtrip") {
          checkM(Gen.int(10, 100).map(s => Refined.unsafeApply[Int, GreaterEqual[W.`10`.T]](s))) { p =>
            val cfg = greaterEqual[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config GreaterEqual invalid") {
          check(Gen.int(1, 9)) { p =>
            val cfg = greaterEqual[W.`10`.T](int("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config Divisible roundtrip") {
          checkM(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, Divisible[W.`10`.T]](s * 10))) { p =>
            val cfg = divisible[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Divisible invalid") {
          check(Gen.int(1, 10).map(_ * 10 + 1)) { p =>
            val cfg = divisible[W.`10`.T](int("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config NonDivisible roundtrip") {
          checkM(Gen.int(1, 10).map(s => Refined.unsafeApply[Int, NonDivisible[W.`10`.T]](s * 10 + 1))) { p =>
            val cfg = nonDivisible[W.`10`.T](int("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config NonDivisible invalid") {
          check(Gen.int(1, 10).map(_ * 10)) { p =>
            val cfg = nonDivisible[W.`10`.T](int("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p.toString), "test"))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        }
      )
    )
