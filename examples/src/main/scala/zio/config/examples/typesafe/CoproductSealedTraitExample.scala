package zio.config.examples.typesafe

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config._, typesafe._

object CoproductSealedTraitExample extends App with EitherImpureOps {

  sealed trait X

  case object A                extends X
  case object B                extends X
  case object C                extends X
  case class D(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  val aHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = a
           |""".stripMargin
      )
      .loadOrThrow

  val bHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = b
           |""".stripMargin
      )
      .loadOrThrow

  val cHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = c
           |""".stripMargin
      )
      .loadOrThrow

  val dHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x {
           |  d {
           |  detail  {
           |    firstName : ff
           |    lastName  : ll
           |    region {
           |      city   : syd
           |      suburb : strath
           |
           |    }
           |  }
           | }
           |}
           |""".stripMargin
      )
      .loadOrThrow

  assert(read(descriptor[X] from aHoconSource) == Right(A))
  assert(read(descriptor[X] from bHoconSource) == Right(B))
  assert(read(descriptor[X] from cHoconSource) == Right(C))
  assert(
    read(descriptor[X] from dHoconSource) == Right(
      D(Detail("ff", "ll", Region("strath", "syd")))
    )
  )
}
