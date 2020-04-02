package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.{ read, _ }

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Long)

  val personConfig =
    (string("name") |@| int("age").optional)(Person.apply, Person.unapply)

  val heightConfig =
    long("height")(Height.apply, Height.unapply)

  val aConfig = nested("any")(personConfig)(A.apply, A.unapply)
  val bConfig = nested("body")(heightConfig)(B.apply, B.unapply)
  val cConfig = boolean("can")(C.apply, C.unapply)
  val dConfig = string("dance")(D.apply, D.unapply)

  val aConfigAsDance: ConfigDescriptor[String, String, Dance] =
    aConfig.xmapEither(
      (a: A) => Right(a: Dance),
      (_: Dance) match {
        case a: A => Right(a)
        case _    => Left("unable to write back")
      }
    )

  val bConfigAsDance: ConfigDescriptor[String, String, Dance] =
    bConfig.xmapEither(
      (a: B) => Right(a: Dance),
      (_: Dance) match {
        case a: B => Right(a)
        case _    => Left("unsable to write back")
      }
    )

  val cConfigAsDance: ConfigDescriptor[String, String, Dance] =
    cConfig.xmapEither(
      (a: C) => Right(a: Dance),
      (_: Dance) match {
        case a: C => Right(a)
        case _    => Left("unsable to write back")
      }
    )

  val dConfigAsDance: ConfigDescriptor[String, String, Dance] =
    dConfig.xmapEither(
      (a: D) => Right(a: Dance),
      (_: Dance) match {
        case a: D => Right(a)
        case _    => Left("unsable to write back")
      }
    )

  val danceConfig =
    aConfigAsDance.orElse(bConfigAsDance).orElse(cConfigAsDance).orElse(dConfigAsDance)

  val aSource = ConfigSource.fromMap(
    Map("any.name" -> "chris"),
    "constant",
    Some('.')
  )

  val bSource = ConfigSource.fromMap(
    Map("body.height" -> "179"),
    "constant",
    Some('.')
  )

  val cSource = ConfigSource.fromMap(
    Map("can" -> "false"),
    "constant",
    Some('.')
  )

  val dSource = ConfigSource.fromMap(
    Map("dance" -> "I am Dancing !!"),
    "constant",
    Some('.')
  )

  val runtime = zio.Runtime.default

  def readA =
    read(danceConfig from aSource)

  def readB =
    read(danceConfig from bSource)

  def readC =
    read(danceConfig from cSource)

  def readD =
    read(danceConfig from dSource)

  val a =
    A(Person("chris", None))

  val b =
    B(Height(179))

  val c =
    C(false)

  val d =
    D("I am Dancing !!")

  assert(
    readA == Right(a) &&
      readB == Right(b) &&
      readC == Right(c) &&
      readD == Right(d)
  )

  val writeA =
    write(danceConfig, a).map(_.flattenString())

  val writeB =
    write(danceConfig, b).map(_.flattenString())

  val writeC =
    write(danceConfig, c).map(_.flattenString())

  val writeD =
    write(danceConfig, d).map(_.flattenString())

  assert(
    writeA == Right(Map("any.name"      -> singleton("chris"))) &&
      writeB == Right(Map("body.height" -> singleton("179"))) &&
      writeC == Right(Map("can"         -> singleton("false"))) &&
      writeD == Right(Map("dance"       -> singleton("I am Dancing !!")))
  )
}
