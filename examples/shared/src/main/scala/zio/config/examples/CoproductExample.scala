package zio.config.examples

import zio.config._

import zio.ZIO
import ConfigDescriptor._

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Long)

  val personConfig: ConfigDescriptor[Person] =
    (string("name") |@| int("age").optional)(Person.apply, Person.unapply)

  val heightConfig: ConfigDescriptor[Height] =
    long("height")(Height.apply, Height.unapply)

  val aConfig: ConfigDescriptor[A] = nested("any")(personConfig)(A.apply, A.unapply)
  val bConfig: ConfigDescriptor[B] = nested("body")(heightConfig)(B.apply, B.unapply)
  val cConfig: ConfigDescriptor[C] = boolean("can")(C.apply, C.unapply)
  val dConfig: ConfigDescriptor[D] = string("dance")(D.apply, D.unapply)

  val danceConfig: ConfigDescriptor[Dance] =
    enumeration[Dance](aConfig, bConfig, cConfig, dConfig)

  val aSource: ConfigSource = zio.config.ConfigSource.fromMap(
    Map("any.name" -> "chris"),
    "constant",
    Some('.')
  )

  val bSource: ConfigSource = ConfigSource.fromMap(
    Map("body.height" -> "179"),
    "constant",
    Some('.')
  )

  val cSource: ConfigSource = ConfigSource.fromMap(
    Map("can" -> "false"),
    "constant",
    Some('.')
  )

  val dSource: ConfigSource = ConfigSource.fromMap(
    Map("dance" -> "I am Dancing !!"),
    "constant",
    Some('.')
  )

  val runtime = zio.Runtime.default

  def readA: ZIO[Any, ReadError[String], Dance] =
    read(danceConfig from aSource)

  def readB: ZIO[Any, ReadError[String], Dance] =
    read(danceConfig from bSource)

  def readC: ZIO[Any, ReadError[String], Dance] =
    read(danceConfig from cSource)

  def readD: ZIO[Any, ReadError[String], Dance] =
    read(danceConfig from dSource)

  val a: A =
    A(Person("chris", None))

  val b: B =
    B(Height(179))

  val c: C =
    C(false)

  val d: D =
    D("I am Dancing !!")

  assert(
    (readA equalM a) &&
      (readB equalM b) &&
      (readC equalM c) &&
      (readD equalM d)
  )

  write(danceConfig, d).map(_.flattenString())

  val writeA: Either[String, Map[String, ::[String]]] =
    write(danceConfig, a).map(_.flattenString())

  val writeB: Either[String, Map[String, ::[String]]] =
    write(danceConfig, b).map(_.flattenString())

  val writeC: Either[String, Map[String, ::[String]]] =
    write(danceConfig, c).map(_.flattenString())

  val writeD: Either[String, Map[String, ::[String]]] =
    write(danceConfig, d).map(_.flattenString())

  assert(
    writeA == Right(Map("any.name" -> singleton("chris"))) &&
      writeB == Right(Map("body.height" -> singleton("179"))) &&
      writeC == Right(Map("can" -> singleton("false"))) &&
      writeD == Right(Map("dance" -> singleton("I am Dancing !!")))
  )

}
