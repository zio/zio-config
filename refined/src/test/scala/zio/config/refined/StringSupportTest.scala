package zio.config.refined

import java.util.UUID

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.{
  EndsWith,
  IPv4,
  MatchesRegex,
  Regex,
  Uri,
  Url,
  Uuid,
  ValidBigDecimal,
  ValidBigInt,
  ValidByte,
  ValidDouble,
  ValidFloat,
  ValidInt,
  ValidLong,
  ValidShort
}
import zio.ZIO
import zio.config.helpers._
import zio.config.{ helpers, BaseSpec, ConfigSource }
import zio.test.Assertion._
import zio.config.{ read, write, ConfigDescriptor, LeafForSequence }, ConfigDescriptor._
import zio.test._

object StringSupportTest
    extends BaseSpec(
      suite("Refined String support")(
        testM("Refined config EndsWith roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, EndsWith[W.`"abc"`.T]](s + "abc"))) { p =>
            val cfg = endsWith[W.`"abc"`.T](string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config EndsWith invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = endsWith[W.`"abc"`.T](string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config IPv4 roundtrip") {
          checkM(Gen.listOfN(4)(Gen.int(0, 255)).map(s => Refined.unsafeApply[String, IPv4](s.mkString(".")))) { p =>
            val cfg = iPv4(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config IPv4 invalid") {
          check(Gen.listOfN(4)(Gen.int(256, 1000)).map(_.mkString("."))) { p =>
            val cfg = iPv4(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config IPv6 invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = iPv6(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config MatchesRegex roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, MatchesRegex[W.`".*abc"`.T]](s + "abc"))) { p =>
            val cfg = matchesRegex[W.`".*abc"`.T](string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config MatchesRegex invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = matchesRegex[W.`".*abc"`.T](string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config Regex roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Regex](s + ".+abc"))) { p =>
            val cfg = regex(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Regex invalid") {
          check(genSymbol(0, 10).map(s => s + "\\q5ab")) { p =>
            val cfg = regex(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config Uri roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Uri]("https://" + s + "w.asdf.asdf"))) { p =>
            val cfg = uri(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Url roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Url]("https://" + s + "w.asdf.asdf"))) { p =>
            val cfg = url(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Uuid roundtrip") {
          checkM(Gen.fromEffect(ZIO.succeed(Refined.unsafeApply[String, Uuid](UUID.randomUUID().toString)))) { p =>
            val cfg = uuid(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config Uuid invalid") {
          check(Gen.fromEffect(ZIO.succeed(UUID.randomUUID().toString + "ab"))) { p =>
            val cfg = uuid(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidByte roundtrip") {
          checkM(Gen.byte(-128, 127).map(s => Refined.unsafeApply[String, ValidByte](s.toString))) { p =>
            val cfg = validByte(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidByte invalid") {
          check(genSymbol(0, 10).map(s => s.toString + "ab")) { p =>
            val cfg = validByte(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidShort roundtrip") {
          checkM(Gen.short(-128, 127).map(s => Refined.unsafeApply[String, ValidShort](s.toString))) { p =>
            val cfg = validShort(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidShort invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validShort(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidInt roundtrip") {
          checkM(Gen.int(-128, 127).map(s => Refined.unsafeApply[String, ValidInt](s.toString))) { p =>
            val cfg = validInt(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidInt invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validInt(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidLong roundtrip") {
          checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidLong](s.toString))) { p =>
            val cfg = validLong(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidLong invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validLong(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidFloat roundtrip") {
          checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidFloat](s.toString + ".123"))) { p =>
            val cfg = validFloat(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidFloat invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validFloat(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidDouble roundtrip") {
          checkM(Gen.double(-128, 127).map(s => Refined.unsafeApply[String, ValidDouble](s.toString))) { p =>
            val cfg = validDouble(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidDouble invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validDouble(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidBigInt roundtrip") {
          checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigInt](s.toString))) { p =>
            val cfg = validBigInt(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidBigInt invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validBigInt(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config ValidBigDecimal roundtrip") {
          checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigDecimal](s.toString + ".123"))) { p =>
            val cfg = validBigDecimal(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread <- ZIO.fromEither(
                           read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("Refined config ValidBigDecimal invalid") {
          check(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = validBigDecimal(string("TEST"))
            val p2 =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assert(p2)(helpers.assertErrors(_.size == 1))
          }
        }
      )
    )
