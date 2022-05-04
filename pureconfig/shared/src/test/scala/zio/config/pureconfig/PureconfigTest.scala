package zio.config.pureconfig

import zio.{Scope, ZIO}
import zio.config._
import zio.test.Assertion._
import zio.test._

object PureconfigTest extends BaseSpec {
  override val spec: Spec[TestEnvironment with Scope, Any] =
    suite("Pureconfig support")(
      test("vector roundtrip") {
        check(Gen.vectorOf(Gen.string)) { v =>
          val cfg = fromPureconfig[Vector[String]]
          val v2  =
            for {
              written <- ZIO.fromEither(write(cfg, v))
              reread  <- read(cfg from ConfigSource.fromPropertyTree(written, "tree"))
            } yield reread

          assertZIO(v2)(equalTo(v))
        }
      }
    )
}
