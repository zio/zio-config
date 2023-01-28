package zio.config.examples.typesafe

import zio.config._

import typesafe._
import magnolia._
import zio._
import zio.config.examples.ZioOps

object TypesafeConfigSealedTrait extends App with EitherImpureOps {
  sealed trait X

  object X {
    case object A                extends X
    case object B                extends X
    case object C                extends X
    case class D(detail: Detail) extends X

    case class Detail(firstName: String, lastName: String, region: Region)
    case class Region(suburb: String, city: String)
  }

  /**
   * We use automatic derivation here.
   * As an example, In order to specify, {{{ x = a }}} in the source where `a`
   * represents X.A object, we need a case class that wraps
   * the sealed trait, and we use the field name of this case class as the key
   */
  final case class Config(x: X)

  import X._

  val aHoconSource: ConfigProvider =
    ConfigProvider
      .fromHoconString(
        s"""
           |x = A
           |""".stripMargin
      )

  val bHoconSource: ConfigProvider =
    ConfigProvider
      .fromHoconString(
        s"""
           |x = B
           |""".stripMargin
      )

  val cHoconSource: ConfigProvider =
    ConfigProvider
      .fromHoconString(
        s"""
           |x = C
           |""".stripMargin
      )

  val dHoconSource: ConfigProvider =
    ConfigProvider
      .fromHoconString(
        s"""
           |x {
           |  D {
           |  detail  {
           |    firstName : ff
           |    lastName  : ll
           |    region {
           |      city   : syd
           |      suburb : strath
           |    }
           |  }
           | }
           |}
           |""".stripMargin
      )

  assert(read(deriveConfig[Config] from aHoconSource) equalM Config(A))
  assert(read(deriveConfig[Config] from bHoconSource) equalM Config(B))
  assert(read(deriveConfig[Config] from cHoconSource) equalM Config(C))
  assert(
    read(deriveConfig[Config] from dHoconSource) equalM
      Config(D(Detail("ff", "ll", Region("strath", "syd"))))
  )
}
