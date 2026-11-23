package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.{Config, ConfigProvider}
import zio.config.magnolia._
import zio.config._
import zio.Unsafe
import zio.IO
import zio.config.syntax._

/**
 * A sample program that loads `A` from a map with `deriveConfig[A].toKebabCase` and prints it. It fails when it starts,
 * because the map lacks the key `a.y.type`.
 */
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
// This fails with "MissingValue path: a.y.type", because `y` has a type with
// `@discriminator("type")` and the map sets `a.y` instead of `a.y.type`. With "a.y.type" -> "HmmAbc" it prints
// A(B(v1,C(),List(C(), C()),None,G(v2),F,G(GValue),Q,T(v3),AbcDef(hello),HmmAbc))
end AutoDerivationSimple
