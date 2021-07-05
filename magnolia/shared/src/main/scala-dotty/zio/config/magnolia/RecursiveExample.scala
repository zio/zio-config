package zio.config.magnolia

import zio.config._, ConfigDescriptor._

final case class RecursiveA(a: B, b: Option[RecursiveA])

object RecursiveA {
  val config: ConfigDescriptor[RecursiveA] =
    (nested("a")(descriptor[B]) |@| nested("b")(config).optional)(RecursiveA.apply, b => Some(b.a, b.b))
}

object RecursiveExample extends App:
  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C, C",
      "a.f.G.value" -> "v2",
      "a.g" -> "D",
      "a.h.G.value" -> "GValue",
      "a.i" -> "Q",
      "a.j.T.u" -> "v3"
    )

  val source =
    ConfigSource.fromMap(
    sourceMap,
    keyDelimiter = Some('.'),
    valueDelimiter = Some(',')
  )

  val result = read(RecursiveA.config from source)

  val expected =
    RecursiveA(B(b = "v1",c = C(), d = List(C(), C()), e = None,f = Right(E.G("v2")), g = E.D, h = E.G("GValue"), i = P.Q, j = P.T("v3")), None)

  assert(result == Right(expected))