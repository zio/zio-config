package zio.config.magnolia.examples

import zio.config._
import zio.{Config, ConfigProvider}
import zio.config.magnolia._
import zio.config.syntax._

final case class Recursive(a: B, b: Option[Recursive])

object RecursiveExample extends App:
  // A mix of automatic derivation + manual derivation
  val config: Config[Recursive] =
    (deriveConfig[B].nested("a") zip config.nested("b").optional).to[Recursive]

  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C, C",
      "a.f.G.value" -> "v2",
      "a.g" -> "D",
      "a.h.G.value" -> "GValue",
      "a.i.p" -> "Q",
      "a.j.p.t.u" -> "v3"
    )

  val source =
    ConfigProvider.fromMap(
    sourceMap
  )

  val result = read_(config from source)

  // val expected =
  //   Recursive(B(b = "v1",c = C(), d = List(C(), C()), e = None,f = Right(E.G("v2")), g = E.D, h = E.G("GValue"), i = P.Q, j = P.T("v3")), None)

  // assert(result == expected)
end RecursiveExample