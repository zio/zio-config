package zio.config.pureconfig

import zio.config._
import zio.test.{Gen, checkM}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object PureconfigTest extends BaseSpec {
  override val spec =
    suite("Pureconfig support")(
      testM("vector roundtrip") {
        checkM(Gen.vectorOf(Gen.anyString)) { v =>
          val cfg = fromPureconfig[Vector[String]]
          val v2  =
            for {
              written <- ZIO.fromEither(write(cfg, v))
              reread  <- ZIO.fromEither(
                read(cfg from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid))
              )
            } yield reread

          assertM(v2)(equalTo(v))
        }
      }
    )
}
