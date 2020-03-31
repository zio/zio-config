---
id: automatic_index
title:  "Automatic Derivation of Config"
---

By bringing in `zio-config-magnolia` you get to avoid all the boilerplate required to define the config.
With a single import, `ConfigDescriptor` is automatically derived for your custom type.

Also, you will find that defining the actual hocon for coproducts is devoid of any extra tagging to satisfy the library.
This is much easier/intuitive unlike many existing implementations.

Take a look at the magnolia examples in `zio-config`

```scala mdoc:silent

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource

object CoproductSealedTraitExample extends App {

  sealed trait X

  case object A                extends X
  case object B                extends X
  case object C                extends X
  case class D(detail: Detail) extends X
  case class Detail(firstName: String, lastName: String, region: Region)
  case class Region(suburb: String, city: String)

  val aHoconSource =
    TypeSafeConfigSource
      .fromHoconString(
        s"""
           |x = a
           |""".stripMargin
      )
      .loadOrThrow

  val bHoconSource =
    TypeSafeConfigSource
      .fromHoconString(
        s"""
           |x = b
           |""".stripMargin
      )
      .loadOrThrow

  val cHoconSource =
    TypeSafeConfigSource
      .fromHoconString(
        s"""
           |x = c
           |""".stripMargin
      )
      .loadOrThrow

  val dHoconSource =
    TypeSafeConfigSource
      .fromHoconString(
        s"""
           |x {
           |  detail  {
           |    firstName : ff
           |    lastName  : ll
           |    region {
           |      city   : syd
           |      suburb : strath
           |
           |    }
           |
           |  }
           |
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

  // Only for example purpose
  implicit class ImpureEither[A, B](either: Either[A, B]) {
     def loadOrThrow: B = either match {
        case Left(_) => throw new Exception()
        case Right(v) => v
      
     }
  }
}

```

### Documentation while automatic derivation
With `describe` annotation you can still document your config while automatically generating the config

```scala mdoc:silent

import zio.config.magnolia.describe

@describe("This config is about aws")
  final case class Aws(region: String, dburl: DbUrl)
  final case class DbUrl(value: String) extends AnyVal

```