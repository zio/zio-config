package zio.config.examples.typesafe

import zio.config._
import zio.config.magnolia.Descriptor

import typesafe._

object PureConfigInterop extends App with EitherImpureOps {

  sealed trait X

  object X {
    case object A                extends X
    case object B                extends X
    case object C                extends X
    case class D(detail: Detail) extends X

    case class Detail(firstName: String, lastName: String, region: Region)
    case class Region(suburb: String, city: String)
  }

  // Note than in pure config, hocon corresponding to case objects need to be
  // "x : { type = A }", while in zio-config that should be "x = A"
  // However for case classes (Example: `D`), both libraries support "x : { type = D, ...}"
  final case class Config(x: X)

  import X._

  val aHoconSource: ConfigSource =
    ConfigSource
      .fromHoconString(
        s"""
           |x = A
           |""".stripMargin
      )

  val bHoconSource: ConfigSource =
    ConfigSource
      .fromHoconString(
        s"""
           |x = B
           |
           |""".stripMargin
      )

  val cHoconSource: ConfigSource =
    ConfigSource
      .fromHoconString(
        s"""
           |x = C
           |""".stripMargin
      )

  val dHoconSource: ConfigSource =
    ConfigSource
      .fromHoconString(
        s"""
           | x : { 
           |  type = D
           |  detail : {
           |    firstName : ff
           |    lastName  : ll
           |    region {
           |      city   : syd
           |      suburb : strath
           |    }
           |  }
           | } 
           |""".stripMargin
      )

  assert(read(Descriptor.descriptorForPureConfig[Config] from aHoconSource) equalM Config(A))
  assert(read(Descriptor.descriptorForPureConfig[Config] from bHoconSource) equalM Config(B))
  assert(read(Descriptor.descriptorForPureConfig[Config] from cHoconSource) equalM Config(C))
  assert(
    read(Descriptor.descriptorForPureConfig[Config] from dHoconSource) equalM
      Config(D(Detail("ff", "ll", Region("strath", "syd"))))
  )
}
