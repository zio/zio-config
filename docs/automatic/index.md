---
id: automatic_index
title:  "Automatic Derivation of Config"
---

By bringing in `zio-config-magnolia` we  avoid all the boilerplate required to define the config.
With a single import, `ConfigDescriptor` is automatically derived.

Also, we will get to see the Hocon config for coproducts is devoid of any extra tagging to satisfy the library.
This is much easier/intuitive unlike many existing implementations.

Take a look at the magnolia examples in `zio-config`. One of them is provided here for quick access.

```scala mdoc:silent

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfigSource
import zio.config.read

object CoproductSealedTraitExample extends App {

  sealed trait X
 
   object X {
     case object A extends X
     case object B extends X
     case object C extends X
     case class  DetailsWrapped(detail: Detail) extends X
     
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
           | details_wrapped {
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

  assert(read(descriptor[Config] from aHoconSource) == Right(A))
  assert(read(descriptor[Config] from bHoconSource) == Right(B))
  assert(read(descriptor[Config] from cHoconSource) == Right(C))
  assert(
    read(descriptor[Config] from dHoconSource) == Right(
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

Please note that, by default the class names in the sealed trait terms will be mapped to their names converted
to snake_case in the config. For example DetailsWrapped in scala code is "details_wrapped" in HOCON.
This is overridable.

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

This will be equivalent to specifying:

```scala
   (string("region") |@| string("dburl").xmap(DbUrl, _.value))(Aws.apply, Aws.unapply) ?? "This config is about aws"
```

### Custom ConfigDescription

Every field in a case class should have an instance of `Descriptor` in order for the automatic derivation to work.
This doesn't mean the entire design of zio-config is typeclass based. For the same reason, the typeclass
Derivation exists only in zio-config-magnolia (or zio-config-shapeless) package.

As an example, given below is a case class where automatic derivation won't work, and result in a compile time error:
Assume that, `AwsRegion` is a type that comes from AWS SDK.

```scala
  import java.time.ZonedDateTime

  case class Execution(time: AwsRegion, id: Int)
```

In this case, `descriptor[Execution]` will give us the following descriptive compile error.

```scala
magnolia: could not find Descriptor.Typeclass for type <outside.library.package>.AwsRegion
  in parameter 'time' of product type <your.packagename>.Execution
```

This is because zio-config-magnolia failed to derive an instance of Descriptor for AwsRegion.

In order to provide implicit instances, following choices are there

```scala
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

 implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
   Descriptor[String].xmap(string => AwsRegion.from(string), _.toString)

```

Now `descriptor[Execution]` compiles.

#### Is that the only way for custom derivation ?

What if our custom type is complex enough that, parsing from a string would actually fail?
The answer is, zio-config provides with all the functions that you need to handle errors.

```scala
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

  implicit val descriptorO: Descriptor[ZonedDateTime] =
    Descriptor[String]
      .xmapEitherELeftPartial(x => Try (ZonedDateTime.parse(x)).toEither)(_.toString)(_.getMessage)
```

What is xmapEitherLeftPartial ? Parsing a String to ZonedDateTime can fail, but converting it back to a string
won't fail. Logically, these are respectively the first 2 functions that we passed to xmapEitherELeftPartial. The third
parameter talks about how to covert the error type E, which in this case is Throwable, to a proper string which is then required
for zio-config to report back to the user what's going on.

PS: We recommend not to use `throwable.getMessage`. Provide more descriptive error message.

You can also rely on `xmapEitherE` if both `to` and `fro` can fail. This is the most commonly used combinator in zio-config.
Similarly if the error type is already String, then you can use `xmapEither` and avoid having to tell the API, how to convert
`E` to `String`. We have already seen these types.

#### Please give descriptions wherever possible for a better experience

Giving descriptions is going to be helpful. While all the built-in types have documentations, it is better we give
some description to custom types as well. For example, its goofd


```scala
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}


  implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
    Descriptor[String]
      .xmap(string => AwsRegion.from(string), _.toString) ?? "value of type AWS.Region"
```

This way, when there is an error due to MissingValue, we get an error message (don't forget to use prettyPrint)
that describes about the config parameter.

##### Where to place these implicits.

If the types are owned by us, then the best place to keep implicit instance is companion object.

```scala

final case clas MyAwsRegion(value: AwsRegion)

object MyCustomTime {
    implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
      Descriptor[String]
        .xmap(string => MyAwsRegion(AwsRegion.from(string)), _.value.toString) ?? "value of type AWS.Region"
}

```

However, most of the time, it's either incovenient, or the types are owned by an external dependency.
In these situations, better off place the implicit closer to where we call the automatic derivation.
Please find the example in `magnolia` package in examples module.

### Change Keys (CamelCase, kebab-case etc)

Please find the examples in ChangeKeys.scala in magnolia module to find how to manipulate
keys in an automatic derivation such as being able to specify keys as camelCase, kebabCase or snakeCase in
the source config.