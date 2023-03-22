package zio.config.magnolia.examples

import zio.config.magnolia._

final case class A(a: B)

final case class B(
  b: String,
  c: C,
  d: List[C],
  e: Option[C],
  f: E,
  g: E,
  h: E,
  i: P,
  j: P,
  z: PureConfigType
)

final case class C()

sealed trait E

object E {
  case object D               extends E
  case object F               extends E
  case class G(value: String) extends E
}

// If a name is provided then the name of the sealed trait itself become part of the config
@name("p")
sealed trait P

object P {
  case object Q                                            extends P
  case object R                                            extends P
  case class S(@name("zz") @describe("it is z") z: String) extends P
  @name("t")
  case class T(u: String)                                  extends P
}

@nameWithLabel("type")
sealed trait PureConfigType

object PureConfigType {
  case class Abc(name: String) extends PureConfigType
}
