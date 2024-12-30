package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.{Config, ConfigProvider}
import zio.config.magnolia._
import zio.config._
import zio.Unsafe
import zio.IO
import zio.config.syntax._

object AutoDerivationSimple extends App:
  // Use of Either is almost prohibited by the looks of it
  val sourceMap =
    Map(
      "a.b-c"       -> "v1",
      "a.c-d"       -> "C",
      "a.d"         -> "C,C",
      "a.f.g.value" -> "v2",
      "a.g"         -> "F",
      "a.h.g.value" -> "GValue",
      "a.i.p"       -> "Q",
      "a.j.p.t.u"   -> "v3",
      "a.z.type"    -> "AbcDef",
      "a.z.name"    -> "hello",
      "a.y"         -> "HmmAbc"
    )

  val source =
    ConfigProvider.fromMap(
      sourceMap,
      pathDelim = ".",
      seqDelim = ","
    )

  val io: IO[String, A] =
    source.load(deriveConfig[A].toKebabCase).mapError(_.prettyPrint())

  val readResult = Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe.run(io).getOrThrowFiberFailure()
  }

  println(readResult)
// A(B(v1,C(),List(C(), C()),None,G(v2),F,G(GValue),Q,T(v3),Abc(hello)))
end AutoDerivationSimple
