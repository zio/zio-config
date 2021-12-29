package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import zio.ZIO
import zio.config.helpers._
import zio.config.{BaseSpec, _}
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object StringSupportTest extends BaseSpec {

  override val spec =
    suite("Refined String support")(
      testM("Refined config IPv4 roundtrip") {
        checkM(Gen.listOfN(4)(Gen.int(0, 255)).map(s => Refined.unsafeApply[String, IPv4](s.mkString(".")))) { p =>
          val cfg = refine[String, IPv4]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config IPv4 invalid") {
        checkM(Gen.listOfN(4)(Gen.int(256, 1000)).map(_.mkString("."))) { p =>
          val cfg = refine[String, IPv4]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config IPv6 invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, IPv6]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config MatchesRegex roundtrip") {
        checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, MatchesRegex[W.`".*abc"`.T]](s + "abc"))) { p =>
          val cfg = refine[String, MatchesRegex[W.`".*abc"`.T]]("TEST")

          val p2 =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config MatchesRegex invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, MatchesRegex[W.`".*abc"`.T]]("TEST")

          val p2 =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config Regex roundtrip") {
        checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Regex](s + ".+abc"))) { p =>
          val cfg = refine[String, Regex]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config Regex invalid") {
        checkM(genSymbol(0, 10).map(s => s + "\\q5ab")) { p =>
          val cfg = refine[String, Regex]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config Uri roundtrip") {
        checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Uri]("https://" + s + "w.asdf.asdf"))) { p =>
          val cfg = refine[String, Uri]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config Url roundtrip") {
        checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, Url]("https://" + s + "w.asdf.asdf"))) { p =>
          val cfg = refine[String, Url]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config Uuid roundtrip") {
        checkM(Gen.fromEffect(ZIO.succeed(Refined.unsafeApply[String, Uuid](UUID.randomUUID().toString)))) { p =>
          val cfg = refine[String, Uuid]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config Uuid invalid") {
        checkM(Gen.fromEffect(ZIO.succeed(UUID.randomUUID().toString + "ab"))) { p =>
          val cfg = refine[String, Uuid]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config ValidByte roundtrip") {
        checkM(Gen.byte(-128, 127).map(s => Refined.unsafeApply[String, ValidByte](s.toString))) { p =>
          val cfg = refine[String, ValidByte]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidByte invalid") {
        checkM(genSymbol(0, 10).map(s => s.toString + "ab")) { p =>
          val cfg = refine[String, ValidByte]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config ValidShort roundtrip") {
        checkM(Gen.short(-128, 127).map(s => Refined.unsafeApply[String, ValidShort](s.toString))) { p =>
          val cfg = refine[String, ValidShort]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidShort invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidShort]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config ValidInt roundtrip") {
        checkM(Gen.int(-128, 127).map(s => Refined.unsafeApply[String, ValidInt](s.toString))) { p =>
          val cfg = refine[String, ValidInt]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidInt invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidInt]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))
        }
      },
      testM("Refined config ValidLong roundtrip") {
        checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidLong](s.toString))) { p =>
          val cfg = refine[String, ValidLong]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidLong invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidLong]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))

        }
      },
      testM("Refined config ValidFloat roundtrip") {
        checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidFloat](s.toString + ".123"))) { p =>
          val cfg = refine[String, ValidFloat]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidFloat invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidFloat]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))

        }
      },
      testM("Refined config ValidDouble roundtrip") {
        checkM(Gen.double(-128, 127).map(s => Refined.unsafeApply[String, ValidDouble](s.toString))) { p =>
          val cfg = refine[String, ValidDouble]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidDouble invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidDouble]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))

        }
      },
      testM("Refined config ValidBigInt roundtrip") {
        checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigInt](s.toString))) { p =>
          val cfg = refine[String, ValidBigInt]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidBigInt invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidBigInt]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))

        }
      },
      testM("Refined config ValidBigDecimal roundtrip") {
        checkM(Gen.long(-128, 127).map(s => Refined.unsafeApply[String, ValidBigDecimal](s.toString + ".123"))) { p =>
          val cfg = refine[String, ValidBigDecimal]("TEST")
          val p2  =
            for {
              written <- ZIO.fromEither(write(cfg, p))
              reread  <-
                read(cfg from ConfigSource.fromPropertyTree(written, "tree"))

            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("Refined config ValidBigDecimal invalid") {
        checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
          val cfg = refine[String, ValidBigDecimal]("TEST")
          val p2  =
            read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

          assertM(p2.mapError(_.size).either)(equalTo(Left(1)))

        }
      }
    )
}
