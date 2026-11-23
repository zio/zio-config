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
  /** The sample's input, with `.` between path segments and `,` between list elements. */
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

  /** A provider that reads `sourceMap`. */
  val source =
    ConfigProvider.fromMap(
      sourceMap,
      pathDelim = ".",
      seqDelim = ","
    )

  /** Loads `A` from `source`, with a failure turned into its `prettyPrint` text. */
  val io: IO[String, A] =
    source.load(deriveConfig[A].toKebabCase).mapError(_.prettyPrint())

  /** The result of running `io`; it throws when loading fails. */
  val readResult = Unsafe.unsafe { implicit u =>
    zio.Runtime.default.unsafe.run(io).getOrThrowFiberFailure()
  }

  println(readResult)
// This fails with a MissingValue error, "Expected a.y.type to be set in properties", because `y` has a type with
// `@discriminator("type")` and the map sets `a.y` instead of `a.y.type`. With "a.y.type" -> "HmmAbc" it prints
// A(B(v1,C(),List(C(), C()),None,G(v2),F,G(GValue),Q,T(v3),AbcDef(hello),HmmAbc))
end AutoDerivationSimple
