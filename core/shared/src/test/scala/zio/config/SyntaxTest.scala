package zio.config

import zio.config.syntax._
import zio.test.Assertion._
import zio.test._
import zio.{ZIO, ZLayer}

object SyntaxTest extends BaseSpec {

  val spec: ZSpec[Environment, Failure] =
    suite("SyntaxTest")(
      test("config.narrow") {
        case class Cfg(a: String, b: Int)
        val cfg = ZLayer.succeed(Cfg("a", 1))
        val a   = ZIO.service[String]

        assertM(a.provideLayer(cfg.narrow(_.a)))(equalTo("a"))
      }
    )
}
