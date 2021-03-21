package zio.config.examples.typesafe

import zio.config.magnolia.{ DeriveConfigDescriptor, Descriptor }
import Descriptor.SealedTraitStrategy._
import zio.config.magnolia.Descriptor.SealedTraitStrategy
import zio.config.typesafe.TypesafeConfigSource
import zio.config.read

object PureConfigInterop extends App with EitherImpureOps {
  val customConfigDescriptor: DeriveConfigDescriptor =
    new DeriveConfigDescriptor {
      override def sealedTraitStrategy: SealedTraitStrategy =
        ignoreSealedTraitName && labelSubClassName("type")
    }

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

  val aHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = A
           |""".stripMargin
      )
      .loadOrThrow

  val bHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = B
           |
           |""".stripMargin
      )
      .loadOrThrow

  val cHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           |x = C
           |""".stripMargin
      )
      .loadOrThrow

  val dHoconSource =
    TypesafeConfigSource
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
      .loadOrThrow

  import customConfigDescriptor._

  assert(read(customConfigDescriptor.descriptor[Config] from aHoconSource) == Right(Config(A)))
  assert(read(customConfigDescriptor.descriptor[Config] from bHoconSource) == Right(Config(B)))
  assert(read(customConfigDescriptor.descriptor[Config] from cHoconSource) == Right(Config(C)))

  assert(
    read(customConfigDescriptor.descriptor[Config] from dHoconSource) == Right(
      Config(D(Detail("ff", "ll", Region("strath", "syd"))))
    )
  )
}
