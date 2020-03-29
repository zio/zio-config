package zio.config.examples.typesafe

import zio.config.ConfigDescriptor._
import zio.config.typesafe.TypeSafeConfigSource
import zio.config.{ read, _ }

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App with EitherImpureOps {

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
    long("height")(Height.apply, Height.unapply)

  val aConfig = nested("any")(personConfig)(A.apply, A.unapply)
  val bConfig = nested("body")(heightConfig)(B.apply, B.unapply)
  val cConfig = boolean("can")(C.apply, C.unapply)
  val dConfig = string("dance")(D.apply, D.unapply)

  // Another much more easier logic is in: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
  // You can also choose to use magnolia module to auto generate if this too much of a boilerplate
  val danceConfig =
    aConfig
      .orElseEither(bConfig)
      .orElseEither(cConfig)
      .orElseEither(dConfig)
      .xmap[Dance](
        {
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
        }, {
          case d @ D(_) => Right(d)
          case c @ C(_) => Left(Right(c))
          case b @ B(_) => Left(Left(Right(b)))
          case a @ A(_) => Left(Left(Left(a)))
        }
      )

  // Don't use loadOrThrow in your codebase. Keep the Either and use for-comprehension
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
