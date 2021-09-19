package zio.config.magnolia.examples

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._

object AutoDerivationSimple extends App :
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
    ConfigSource.fromMap(
    sourceMap,
    keyDelimiter = Some('.'),
    valueDelimiter = Some(',')
  )

  val desc = descriptor[A]

  val readResult = read(desc from source)

  val expected =
    A(B(b = "v1",c = C(), d = List(C(), C()), e = None,f = Right(E.G("v2")), g = E.D, h = E.G("GValue"), i = P.Q, j = P.T("v3")))

  assert(readResult == Right(expected))

  val writeResult =
    write(desc, expected).map(_.flattenKeyAndValue(valueDelimiter = ", "))

  assert(writeResult.map(_.toList.sortBy(_._1)) == Right(sourceMap.toList.sortBy(_._1)))
end AutoDerivationSimple
