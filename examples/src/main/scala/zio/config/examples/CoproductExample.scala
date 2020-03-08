package zio.config.examples

import zio.DefaultRuntime
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
    long("height").xmap(Height)(_.height)

  val aConfig = nested("any")(personConfig).xmap(A)(_.any)
  val bConfig = nested("body")(heightConfig).xmap(B)(_.body)
  val cConfig = boolean("can").xmap(C)(_.can)
  val dConfig = string("dance").xmap(D)(_.dance)

  val aConfigAsDance =
    aConfig.xmapEither(a => Right(a: Dance))({
      case a: A => Right(a)
      case _    => Left("unable to write back")
    })

  val bConfigAsDance =
    bConfig.xmapEither(a => Right(a: Dance))({
      case a: B => Right(a)
      case _    => Left("unsable to write back")
    })

  val cConfigAsDance =
    cConfig.xmapEither(a => Right(a: Dance))({
      case a: C => Right(a)
      case _    => Left("unsable to write back")
    })

  val dConigAsDance =
    dConfig.xmapEither(a => Right(a: Dance))({
      case a: D => Right(a)
      case _    => Left("unsable to write back")
    })

  val danceConfig =
    aConfigAsDance.orElse(bConfigAsDance).orElse(cConfigAsDance).orElse(dConigAsDance)

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
