package zio.config.examples

import zio.ZIO
import zio.config._

import Config._

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Long)

  val personConfig: Config[Person] =
    (string("name") zip int("age").optional).to[Person]

  val heightConfig: Config[Height] =
    long("height").to[Height]

  val aConfig: Config[A] = nested("any")(personConfig).to[A]
  val bConfig: Config[B] = nested("body")(heightConfig).to[B]
  val cConfig: Config[C] = boolean("can").to[C]
  val dConfig: Config[D] = string("dance").to[D]

  val danceConfig: Config[Dance] =
    enumeration[Dance](aConfig, bConfig, cConfig, dConfig)

  val aSource: ConfigSource = zio.config.ConfigProvider.fromMap(
    Map("any.name" -> "chris"),
    "constant",
    Some('.')
  )

  val bSource: ConfigSource = ConfigProvider.fromMap(
    Map("body.height" -> "179"),
    "constant",
    Some('.')
  )

  val cSource: ConfigSource = ConfigProvider.fromMap(
    Map("can" -> "false"),
    "constant",
    Some('.')
  )

  val dSource: ConfigSource = ConfigProvider.fromMap(
    Map("dance" -> "I am Dancing !!"),
    "constant",
    Some('.')
  )

  val runtime = zio.Runtime.default

  def readA: ZIO[Any, Config.Error, Dance] =
    read(danceConfig from aSource)

  def readB: ZIO[Any, Config.Error, Dance] =
    read(danceConfig from bSource)

  def readC: ZIO[Any, Config.Error, Dance] =
    read(danceConfig from cSource)

  def readD: ZIO[Any, Config.Error, Dance] =
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
