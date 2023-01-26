package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.{Config, ConfigProvider}
import zio.config.syntax._
import zio.config.magnolia._
import zio.config._
import zio.Unsafe

object AutoDerivationSimple extends App :
  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C,C",
      "a.f.G" -> "v2",
      "a.g" -> "D",
      "a.h.G" -> "GValue",
      "a.i.p" -> "Q",
      "a.j.p.t.u" -> "v3"
    )

  val source =
    ConfigProvider.fromMap(
    sourceMap,
    pathDelim = ".",
    seqDelim = ","
  )

  val desc = deriveConfig[A]

  val readResult = Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe.run(read_(desc from source)).getOrThrowFiberFailure()
  }
    
  println(readResult)

  val expected =
    A(B("v1",C(), List(C(), C()), e = None,f = Right(E.G("v2")), g = E.D, h = E.G("GValue"), i = P.Q, j = P.T("v3")))

  assert(readResult == expected)
end AutoDerivationSimple
