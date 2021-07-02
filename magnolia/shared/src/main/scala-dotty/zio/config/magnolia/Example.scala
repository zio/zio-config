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

@name("E")
enum E:
  case D
  case F
  @name("G")
  case G(value: String)

enum P:
  case Q
  case R
  case S(z: String)
  @name("t")
  case T(u: String)

@label("type")
enum V:
  case X
  case Y
  case Z

object Example extends App :
  val source =
    ConfigSource.fromMap(
      Map(
        "a.b" -> "v1",
        "a.c" -> "C",
        "a.d" -> "C, C",
        "a.f.E.G.value" -> "v2",
        "a.g.E" -> "D",
        "a.h.E" -> "F",
        "a.h" -> "Q",
        "a.i" -> "Q",
        "a.j.t.u" -> "v3"
      ),
      keyDelimiter = Some('.'),
      valueDelimiter = Some(',')
    )

  val res = read(descriptor[A] from source)

  println(res)
  // Right(A(B(hi,C(),List(C(), C()),None,Right(F))))

end Example
