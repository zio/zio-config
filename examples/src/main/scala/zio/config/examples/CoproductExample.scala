package zio.config.examples

import zio.config.magnolia.ConfigDescriptorProvider._
import zio.config._, ConfigDescriptor._
import zio.DefaultRuntime

object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Long)

  val descrip = description[Dance]

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

  val runtime = new DefaultRuntime {}

  def readA =
    runtime.unsafeRun(
      read(descrip from aSource)
    )

  def readB =
    runtime.unsafeRun(
      read(descrip from bSource)
    )

  def readC =
    runtime.unsafeRun(
      read(descrip from cSource)
    )

  def readD =
    runtime.unsafeRun(
      read(descrip from dSource)
    )

  assert(
    readA == A(Person("chris", None)) &&
      readB == B(Height(179)) &&
      readC == C(false) &&
      readD == D("I am Dancing !!")
  )

  val writeA =
    write(descrip, readA).map(_.flattenString())

  val writeB =
    write(descrip, readB).map(_.flattenString())

  val writeC =
    write(descrip, readC).map(_.flattenString())

  val writeD =
    write(descrip, readD).map(_.flattenString())

  assert(
    writeA == Right(Map("any.name"      -> singleton("chris"))) &&
      writeB == Right(Map("body.height" -> singleton("179"))) &&
      writeC == Right(Map("can"         -> singleton("false"))) &&
      writeD == Right(Map("dance"       -> singleton("I am Dancing !!")))
  )
}
