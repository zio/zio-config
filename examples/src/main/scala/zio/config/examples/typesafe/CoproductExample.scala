/*
package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor._
import zio.config.typesafe.TypeSafeConfigSource.hocon
import zio.config.{ read, _ }

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App {

  sealed trait Dance

  case class A(any: Person) extends Dance

  case class B(body: Height) extends Dance

  case class C(can: Boolean) extends Dance

  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])

  final case class Height(height: Long)

  val personConfig =
    (string("name") |@| int("age").optional)(Person.apply, Person.unapply)

  val heightConfig =
    long("height").xmap(Height)(_.height)

  val aConfig = nested("any")(personConfig).xmap(A)(_.any)
  val bConfig = nested("body")(heightConfig).xmap(B)(_.body)
  val cConfig = boolean("can").xmap(C)(_.can)
  val dConfig = string("dance").xmap(D)(_.dance)

  // Another much more easier logic is in: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
  // You can also choose to use magnolia module to auto generate if this too much of a boilerplate
  val danceConfig =
    aConfig
      .orElseEither(bConfig)
      .orElseEither(cConfig)
      .orElseEither(dConfig)
      .xmap({
        case Right(value) => value: Dance
        case Left(value) =>
          value match {
            case Right(value) => value: Dance
            case Left(value) =>
              value match {
                case Right(value) => value: Dance
                case Left(value)  => value: Dance
              }
          }
      })({
        case d @ D(_) => Right(d)
        case c @ C(_) => Left(Right(c))
        case b @ B(_) => Left(Left(Right(b)))
        case a @ A(_) => Left(Left(Left(a)))
      })

  val aSource = hocon(
    Right(
      "any.name = chris"
    )
  )

  val bSource = hocon(
    Right(
      "body.height = 179"
    )
  )

  val cSource = hocon(
    Right(
      "can = false"
    )
  )

  val dSource = hocon(
    Right(
      """dance = "I am Dancing !!""""
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
 */
