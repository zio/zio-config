/*
package zio.config.examples.magnolia

import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.{ read, singleton, write, ConfigSource, ReadError }

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

  def readA: Either[ReadError, Dance] =
    read(danceConfig from aSource)

  def readB: Either[ReadError, Dance] =
    read(danceConfig from bSource)

  def readC: Either[ReadError, Dance] =
    read(danceConfig from cSource)

  def readD: Either[ReadError, Dance] =
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
 */
