package zio.config.examples.magnolia

import zio.DefaultRuntime
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.{ConfigSource, read, singleton, write}

object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Long)

  val danceConfig = description[Dance]

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
