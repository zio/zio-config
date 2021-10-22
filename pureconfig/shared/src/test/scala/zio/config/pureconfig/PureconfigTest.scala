package zio.config.pureconfig

import zio.{Has, Random, ZIO}
import zio.config._
import zio.test.Assertion._
import zio.test.{Gen, _}

object PureconfigTest extends BaseSpec {
  override val spec: Spec[Has[TestConfig] with Has[Random] with Has[Sized], TestFailure[Serializable], TestSuccess] =
    suite("Pureconfig support")(
      test("vector roundtrip") {
        check(Gen.vectorOf(Gen.string)) { v =>
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
