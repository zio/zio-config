package zio.config.magnolia

import zio.config.magnolia._
import zio.config._, ConfigDescriptor._

final case class A(a: B)

final case class B(
  b: String,
  c: C,
  d: List[C],
  e: Option[C],
  f: Either[C, E],
  g: E,
  h: E,
  i: P,
  j: P
)

final case class C()

sealed trait E

object E {
  case object D extends E
  case object F extends E
  case class G(value: String) extends E
}

// If a name is provided then the name of the sealed trait itself become part of the config
@name("p")
sealed trait P

object P {
  case object Q extends P
  case object R extends P
  case class S(z: String) extends P
  case class T(u: String) extends P
}

  // Right(A(B(v1,C(),List(C(), C()),None,Right(G(v2)),D,G(GValue),Q,T(v3))))
object Example extends App :
  val sourceMap =
    Map(
      "a.b" -> "v1",
      "a.c" -> "C",
      "a.d" -> "C, C",
      "a.f.G.value" -> "v2",
      "a.g" -> "D",
      "a.h.G.value" -> "GValue",
      "a.i.p" -> "Q",
      "a.j.p.T.u" -> "v3"
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

end Example
