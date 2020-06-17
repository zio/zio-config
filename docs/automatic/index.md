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
import zio.config.typesafe.TypesafeConfigSource

object CoproductSealedTraitExample extends App {

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

There are various ways in which you can customise the derivation of sealed traits. 
This is a bit involving, and more documentations will be provided soon.

### Documentation while automatic derivation
With `describe` annotation you can still document your config while automatically generating the config

```scala mdoc:silent

import zio.config.magnolia.describe

@describe("This config is about aws")
case class Aws(region: String, dburl: DbUrl)
case class DbUrl(value: String)

```

### Custom ConfigDescription

The way automatic derivation works is that every field in your config class should have an instance of the typeclass
Descriptor. This doesn't mean the entire design of zio-config is typeclass based. For the same reason, the typeclass
Derivation exists only in zio-config-magnolia (or zio-config-shapeless) package.

As an example, below given is a case class where automatic derivation won't work, and result in a compile time error:
Given, `AwsRegion` is a type that comes from AWS SDK.

```scala
  import java.time.ZonedDateTime

  case class Execution(time: AwsRegion, id: Int)
```

In this case, `descriptor[Execution]` will give us the following descriptive compile error.

```scala
magnolia: could not find Descriptor.Typeclass for type <outside.library.package>.AwsRegion
  in parameter 'time' of product type <packagename>.Execution
```

This is because zio-config-magnolia failed to derive an instance of Descriptor for AwsRegion.

In order to provide implicit instances, following choices are there

```
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

 implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
   Descriptor[String].xmap(string => AwsRegion.from(string), _.toString)

```

Now `descriptor[Execution]` compiles.

#### Is that the only way for custom derivation.

Well, what if our custom type is complex enough that, parsing from a string would actually fail?
The answer is, zio-config provides with all the functions that you need to handle errors.

```scala mdoc:silent
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

  implicit val descriptorO: Descriptor[ZonedDateTime] =
    Descriptor[String]
      .xmapEitherELeftPartial(x => Try (ZonedDateTime.parse(x)).toEither)(_.toString)(_.getMessage)
```

So, what is xmapEitherLeftPartial ? Yes, parsing a String to ZonedDateTime can fail, but converting it bacck to a string
won't fail. Logically, these are respectively the first 2 functions that you passed to xmapEitherELeftPartial. The third
parameter talks about how to covert the error type E, which in this case is Throwable to a proper string, which is required
for zio-config to report back to the user what's going on.

PS: We recommend not to use `throwable.getMessage`. Provide more descriptive error message.

You can also rely on `xmapEitherE` if both `to` and `fro` can fail. This is the most commonly used combinator in zio-config.
Similary if your error type is already String, then you can use `xmapEither` and avoid having to tell the API, how to convert
`E` to `String`. 

#### Please give descriptions wherever possible for a better experience

Giving descriptions is going to be helpful. While all the built-in types have documentations, it is better we give
some description to custom types as well. For example, its goofd


```scala mdoc:silent
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}


  implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
    Descriptor[String]
      .xmap(string => AwsRegion.from(string), _.toString) ?? "value of type AWS.Region"
```

This way, when there is an error due to MissingValue, we get an error message (don't forget to use prettyPrint)
that describes about the config parameter.