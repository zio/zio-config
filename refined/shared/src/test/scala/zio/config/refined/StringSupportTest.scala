package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import zio.ZIO
import zio.config.helpers._
import zio.config.{BaseSpec, helpers, _}
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object StringSupportTest extends BaseSpec {

  override val spec =
    suite("Refined String support")(
      test("Refined config IPv4 roundtrip") {
        check(Gen.listOfN(4)(Gen.int(0, 255)).map(s => Refined.unsafeApply[String, IPv4](s.mkString(".")))) { p =>
          val cfg = refine[String, IPv4]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config IPv4 invalid") {
        check(Gen.listOfN(4)(Gen.int(256, 1000)).map(_.mkString("."))) { p =>
          val cfg = refine[String, IPv4]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config IPv6 invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, IPv6]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config MatchesRegex roundtrip") {
        check(genSymbol(0, 10).map(s => Refined.unsafeApply[String, MatchesRegex[W.`".*abc"`.T]](s + "abc"))) { p =>
          val cfg = refine[String, MatchesRegex[W.`".*abc"`.T]]("TEST")

          val p2 =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config MatchesRegex invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, MatchesRegex[W.`".*abc"`.T]]("TEST")

          val p2 =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config Regex roundtrip") {
        check(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Regex](s + ".+abc"))) { p =>
          val cfg = refine[String, Regex]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Regex invalid") {
        check(genSymbol(0, 10).map(s => s + "\\q5ab")) { p =>
          val cfg = refine[String, Regex]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config Uri roundtrip") {
        check(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Uri]("https://" + s + "w.asdf.asdf"))) { p =>
          val cfg = refine[String, Uri]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Url roundtrip") {
        check(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Url]("https://" + s + "w.asdf.asdf"))) { p =>
          val cfg = refine[String, Url]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Uuid roundtrip") {
        check(Gen.fromZIO(ZIO.succeed(Refined.unsafeApply[String, Uuid](UUID.randomUUID().toString)))) { p =>
          val cfg = refine[String, Uuid]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config Uuid invalid") {
        check(Gen.fromZIO(ZIO.succeed(UUID.randomUUID().toString + "ab"))) { p =>
          val cfg = refine[String, Uuid]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidByte roundtrip") {
        check(Gen.byte(-128, 127).map(s => Refined.unsafeApply[String, ValidByte](s.toString))) { p =>
          val cfg = refine[String, ValidByte]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidByte invalid") {
        check(genSymbol(0, 10).map(s => s.toString + "ab")) { p =>
          val cfg = refine[String, ValidByte]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidShort roundtrip") {
        check(Gen.short(-128, 127).map(s => Refined.unsafeApply[String, ValidShort](s.toString))) { p =>
          val cfg = refine[String, ValidShort]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidShort invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidShort]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidInt roundtrip") {
        check(Gen.int(-128, 127).map(s => Refined.unsafeApply[String, ValidInt](s.toString))) { p =>
          val cfg = refine[String, ValidInt]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidInt invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidInt]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidLong roundtrip") {
        check(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidLong](s.toString))) { p =>
          val cfg = refine[String, ValidLong]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidLong invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidLong]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidFloat roundtrip") {
        check(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidFloat](s.toString + ".123"))) { p =>
          val cfg = refine[String, ValidFloat]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidFloat invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidFloat]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidDouble roundtrip") {
        check(Gen.double(-128, 127).map(s => Refined.unsafeApply[String, ValidDouble](s.toString))) { p =>
          val cfg = refine[String, ValidDouble]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidDouble invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidDouble]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidBigInt roundtrip") {
        check(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigInt](s.toString))) { p =>
          val cfg = refine[String, ValidBigInt]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidBigInt invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidBigInt]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      },
      test("Refined config ValidBigDecimal roundtrip") {
        check(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigDecimal](s.toString + ".123"))) { p =>
          val cfg = refine[String, ValidBigDecimal]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("Refined config ValidBigDecimal invalid") {
        check(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidBigDecimal]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assert(p2)(helpers.assertErrors(_.size == 1))
        }
      }
    )
}
