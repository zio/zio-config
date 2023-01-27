package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.{Config, ConfigProvider}
import zio.config.magnolia._
import zio.config._
import zio.Unsafe

object AutoDerivationSimple extends App :
  // Use of Either is almost prohibited by the looks of it
  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C,C",
      "a.f.G.value" -> "v2",
      "a.g" -> "afsak",
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
  
  val desc = deriveConfig[A]

  val readResult = Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe.run(read(desc from source)).getOrThrowFiberFailure()
  }

  println(readResult)
end AutoDerivationSimple
