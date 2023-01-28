package zio.config.examples

import zio.ZIO
import zio.config._
import zio.config.magnolia._

import zio.Config, Config._
import zio.ConfigProvider

// see Stackoverflow: https://stackoverflow.com/questions/59670366/how-to-handle-an-adt-sealed-trait-with-zio-config
object CoproductExample extends App {

  sealed trait Dance
  case class A(any: Person)   extends Dance
  case class B(body: Height)  extends Dance
  case class C(can: Boolean)  extends Dance
  case class D(dance: String) extends Dance

  final case class Person(name: String, age: Option[Int])
  final case class Height(height: Int)

  val personConfig: Config[Person] =
    (string("name") zip int("age").optional).to[Person]

  val heightConfig: Config[Height] =
    int("height").to[Height]

  val aConfig: Config[A] = (personConfig.nested("any")).to[A]
  val bConfig: Config[B] = (heightConfig.nested("body")).to[B]
  val cConfig: Config[C] = boolean("can").to[C]
  val dConfig: Config[D] = string("dance").to[D]

  val danceConfig: Config[Dance] =
    deriveConfig[Dance]

  val aSource: ConfigProvider = ConfigProvider.fromMap(
    Map("any.name" -> "chris")
  )

  val bSource: ConfigProvider = ConfigProvider.fromMap(
    Map("body.height" -> "179")
  )

  val cSource: ConfigProvider = ConfigProvider.fromMap(
    Map("can" -> "false")
  )

  val dSource: ConfigProvider = ConfigProvider.fromMap(
    Map("dance" -> "I am Dancing !!")
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

}
