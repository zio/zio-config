package zio.config

import zio.{ Has, ZIO, ZLayer }
import zio.config.syntax._
import zio.test.Assertion._
import zio.test._

object SyntaxTest
    extends BaseSpec(
      suite("SyntaxTest")(
        testM("config.narrow") {
          case class Cfg(a: String, b: Int)
          val cfg = ZLayer.succeed(Cfg("a", 1))
          val a   = ZIO.access[Has[String]](_.get)

          assertM(a.provideLayer(cfg.narrow(_.a)))(equalTo("a"))
        }
      )
    )
