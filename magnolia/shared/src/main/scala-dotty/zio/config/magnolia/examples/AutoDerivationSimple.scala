package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.{Config, ConfigProvider}
import zio.config.magnolia._
import zio.config._
import zio.Unsafe
import zio.IO

object AutoDerivationSimple extends App :
  // Use of Either is almost prohibited by the looks of it
  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C,C",
      "a.f.G.value" -> "v2",
      "a.g" -> "F",
      "a.h.G.value" -> "GValue",
      "a.i.p" -> "Q",
      "a.j.p.t.u" -> "v3"
    )

  val source =
    ConfigProvider.fromMap(
    sourceMap,
    pathDelim = ".",
    seqDelim = ","
  )

  val io: IO[Config.Error, A] = 
    source.autoLoad[A]  

  val readResult = Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe.run(io).getOrThrowFiberFailure()
  }

  println(readResult)
end AutoDerivationSimple
