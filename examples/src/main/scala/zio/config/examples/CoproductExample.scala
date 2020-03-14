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
    long("height").xmap(Height, (_: Height).height)

  val aConfig = nested("any")(personConfig).xmap(A, (_: A).any)
  val bConfig = nested("body")(heightConfig).xmap(B, (_: B).body)
  val cConfig = boolean("can").xmap(C, (_: C).can)
  val dConfig = string("dance").xmap(D, (_: D).dance)

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
    Map(
      "any.name" -> "chris"
    )
  )

  val bSource = ConfigSource.fromMap(
    Map(
      "body.height" -> "179"
    )
  )

  val cSource = ConfigSource.fromMap(
    Map(
      "can" -> "false"
    )
  )

  val dSource = ConfigSource.fromMap(
    Map(
      "dance" -> "I am Dancing !!"
    )
  )

  val runtime = zio.Runtime.default

  def readA =
    runtime.unsafeRun(
      read(danceConfig from aSource)
    )

  def readB =
    runtime.unsafeRun(
      read(danceConfig from bSource)
    )

  def readC =
    runtime.unsafeRun(
      read(danceConfig from cSource)
    )

  def readD =
    runtime.unsafeRun(
      read(danceConfig from dSource)
    )

  assert(
    readA == A(Person("chris", None)) &&
      readB == B(Height(179)) &&
      readC == C(false) &&
      readD == D("I am Dancing !!")
  )

  val writeA =
    write(danceConfig, readA).map(_.flattenString())

  val writeB =
    write(danceConfig, readB).map(_.flattenString())

  val writeC =
    write(danceConfig, readC).map(_.flattenString())

  val writeD =
    write(danceConfig, readD).map(_.flattenString())

  assert(
    writeA == Right(Map("any.name"      -> singleton("chris"))) &&
      writeB == Right(Map("body.height" -> singleton("179"))) &&
      writeC == Right(Map("can"         -> singleton("false"))) &&
      writeD == Right(Map("dance"       -> singleton("I am Dancing !!")))
  )
}
