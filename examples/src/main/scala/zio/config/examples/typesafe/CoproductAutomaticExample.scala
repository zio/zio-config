package zio.config.examples.typesafe

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.{ TypeSafeConfigSource }
import zio.config.{ read, _ }

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductAutomaticExample extends App with EitherImpureOps {

  sealed trait Dance

  case class A(any: Person) extends Dance

  case class B(body: Height) extends Dance

  case class C(can: Boolean) extends Dance

  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])

  final case class Height(height: Long)

  // Don't use loadOrThrow in your code base, This is only in examples
  val danceConfig = descriptor[Dance]

  val aSource = TypeSafeConfigSource.fromHoconString("any.name = chris").loadOrThrow

  val bSource = TypeSafeConfigSource.fromHoconString("body.height = 179").loadOrThrow

  val cSource = TypeSafeConfigSource.fromHoconString("can = false").loadOrThrow

  val dSource = TypeSafeConfigSource.fromHoconString("""dance = "I am Dancing !!"""").loadOrThrow

  def readA =
    read(danceConfig from aSource).loadOrThrow

  def readB =
    read(danceConfig from bSource).loadOrThrow

  def readC =
    read(danceConfig from cSource).loadOrThrow

  def readD =
    read(danceConfig from dSource).loadOrThrow

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
