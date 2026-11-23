package zio.config.magnolia.examples

import zio.config.magnolia._

/** The sample's root config: a `B` under the key `a`. */
final case class A(a: B)

/**
 * A sample case class with fields of several kinds: text, a case class without fields, a list, an option and sealed
 * traits.
 */
final case class B(
  bC: String,
  cD: C,
  d: List[C],
  e: Option[C],
  f: E,
  g: E,
  h: E,
  i: P,
  j: P,
  z: PureConfigType,
  y: PureConfigType
)

/** A case class without fields, read as its name. */
final case class C()

/** A sample sealed trait without annotations. */
sealed trait E

/** The subtypes of `E`. */
object E {

  /** A case object subtype of `E`. */
  case object D extends E

  /** A case object subtype of `E`. */
  case object F extends E

  /** A case class subtype of `E`, with one field. */
  case class G(value: String) extends E
}

// If a name is provided then the name of the sealed trait itself become part of the config
/** A sample sealed trait with `@name("p")`, so its subtypes are read under one more key, `p`. */
@name("p")
sealed trait P

/** The subtypes of `P`. */
object P {

  /** A case object subtype of `P`. */
  case object Q extends P

  /** A case object subtype of `P`. */
  case object R extends P

  /** A case class subtype of `P`, with one field. */
  case class S(z: String) extends P

  /** A case class subtype of `P`, with one field. */
  case class T(u: String) extends P
}

/** A sample sealed trait with `@discriminator("type")`: the key `type` names the subtype. */
@discriminator("type")
sealed trait PureConfigType

/** The subtypes of `PureConfigType`. */
object PureConfigType {

  /** A case class subtype of `PureConfigType`, with one field. */
  case class AbcDef(name: String) extends PureConfigType

  /** A case object subtype of `PureConfigType`. */
  case object HmmAbc extends PureConfigType
}
