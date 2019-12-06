package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.{ EndsWith, IPv4 }
import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.helpers._
import zio.config.{ helpers, read, write, BaseSpec, ConfigSource, ReadErrorsVector }
import zio.test.Assertion._
import zio.test._

object StringSupportTest
    extends BaseSpec(
      suite("Refined support")(
        testM("Refined config EndsWith roundtrip") {
          checkM(genSymbol(0, 10).map(s => Refined.unsafeApply[String, EndsWith[W.`"abc"`.T]](s + "abc"))) { p =>
            val cfg = endsWith[W.`"abc"`.T](string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread  <- read(cfg from ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("Refined config EndsWith invalid") {
          checkM(genSymbol(0, 10).map(s => s + "ab")) { p =>
            val cfg = endsWith[W.`"abc"`.T](string("TEST"))
            val p2: ZIO[Any, ReadErrorsVector[String, String], Refined[String, EndsWith[W.`"abc"`.T]]] =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assertM(p2.either, helpers.assertErrors(_.size == 1))
          }
        },
        testM("Refined config IPv4 roundtrip") {
          checkM(Gen.listOfN(4)(Gen.int(0, 255)).map(s => Refined.unsafeApply[String, IPv4](s.mkString(".")))) { p =>
            val cfg = iPv4(string("TEST"))
            val p2 =
              for {
                written <- ZIO.fromEither(write(cfg, p))
                reread  <- read(cfg from ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("Refined config IPv4 invalid") {
          checkM(Gen.listOfN(4)(Gen.int(256, 1000)).map(_.mkString("."))) { p =>
            val cfg = iPv4(string("TEST"))
            val p2: ZIO[Any, ReadErrorsVector[String, String], Refined[String, IPv4]] =
              read(cfg from ConfigSource.fromMap(Map("TEST" -> p)))

            assertM(p2.either, helpers.assertErrors(_.size == 1))
          }
        }
      )
    )
