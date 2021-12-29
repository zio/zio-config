package zio.config.pureconfig

import zio.ZIO
import zio.config._
import zio.test.Assertion._
import zio.test.{Gen, checkM, _}
import zio.random.Random

object PureconfigTest extends BaseSpec {
  override val spec: Spec[TestConfig with Random with Sized, TestFailure[Serializable], TestSuccess] =
    suite("Pureconfig support")(
      testM("vector roundtrip") {
        checkM(Gen.vectorOf(Gen.anyString)) { v =>
          val cfg = fromPureconfig[Vector[String]]
          val v2  =
            for {
              written <- ZIO.fromEither(write(cfg, v))
              reread  <- read(cfg from ConfigSource.fromPropertyTree(written, "tree"))
            } yield reread

          assertM(v2)(equalTo(v))
        }
      }
    )
}
