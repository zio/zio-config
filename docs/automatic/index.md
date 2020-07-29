---
id: automatic_index
title:  "Automatic Derivation of ConfigDescriptor"
---

By bringing in `zio-config-magnolia` we  avoid all the boilerplate required to define the config.
With a single import, `ConfigDescriptor` is automatically derived.

Also, we will get to see the Hocon config for coproducts is devoid of any extra tagging to satisfy the library.
This is much easier/intuitive unlike many existing implementations.

Take a look at the magnolia examples in `zio-config`. One of them is provided here for quick access.

Note:  `zio-config-shapeless` is an alternative to `zio-config-magnolia` to support scala 2.11 projects. 
It will be deprecated once we find users have moved on from scala 2.11. 

### Example

#### Config
```scala


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
  case class MyConfig(x: X)
```

#### AutoDerivation
```scala
  // Setting up imports

  import zio.config._, zio.config.typesafe._
  import zio.config.magnolia.DeriveConfigDescriptor._

  import X._
```

```scala
  // Only for example purpose
  implicit class ImpureEither[A, B](either: Either[A, B]) {
    def loadOrThrow: B = either match {
      case Left(_) => throw new Exception()
      case Right(v) => v
     }
  }
```

```scala

  // Defining different possibility of HOCON source

  val aHoconSource =
    TypesafeConfigSource
      .fromHoconString("x = a")
      .loadOrThrow
```

```scala
  val bHoconSource =
    TypesafeConfigSource
      .fromHoconString("x = b")
      .loadOrThrow
```

```scala
  val cHoconSource =
    TypesafeConfigSource
      .fromHoconString("x = c")
      .loadOrThrow
```

```scala
  val dHoconSource =
    TypesafeConfigSource
      .fromHoconString(
        s"""
           | x {
           |   details_wrapped {
           |    detail  {
           |      firstName : ff
           |      lastName  : ll
           |      region {
           |        city   : syd
           |        suburb : strath
           |     }
           |   }
           |  }
           |}
           |""".stripMargin
      )
      .loadOrThrow

```

```scala
  // Let's try automatic derivation

  read(descriptor[MyConfig] from aHoconSource)
  // res0: Right(MyConfig(A))

  read(descriptor[MyConfig] from bHoconSource)
  // res0: Right(MyConfig(B))

  read(descriptor[MyConfig] from cHoconSource)
  // res0: Right(MyConfig(C))

  read(descriptor[MyConfig] from dHoconSource)
  // res0: Right(MyConfig(DetailsWrapped(Detail("ff", "ll", Region("strath", "syd")))))

```

**NOTE**

By default the class names in the sealed trait terms will be mapped to their names converted
to snake_case in the config. 

For example `details_wrapped` in the above HOCON string corresponds to `DetailsWrapped` which is the name of the case class in Scala code.
This is overridable. 

The fieldNames remain as it is as you can see. You can override this one as well.
They can be easily changed using `mapKey`. Note that, fieldName is not equal to className. So the `DetailsWrapped`
in Scala code has to be `details_wrapped` in HOCON even after the below code.

If you want custom names for your fields, use `name` annotation.

```
  import zio.config.derivation.name

  @name("detailsWrapped")
  case class  DetailsWrapped(detail: Detail) extends X

```

```scala
import zio.config._

descriptor[MyConfig].mapKey(toKebabCase)

```

With the above change`firstName` and `lastName` in the above HOCON example can be `first-name` and `last-name` 
respectively.

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


This will be equivalent to the manual configuration of:

```scala
   (string("region") |@| string("dburl").transform(DbUrl, _.value))(Aws.apply, Aws.unapply) ?? "This config is about aws"
```

You could provide `describe` annotation at field level

Example:

```scala
case class Aws(@describe("AWS region") region: String, dburl: DbUrl)
```

This will be equivalent to the manual configuration of:


```scala
   (string("region") ?? "AWS region" |@| string("dburl").transform(DbUrl, _.value))(Aws.apply, Aws.unapply) ?? "This config is about aws"
```



### Custom ConfigDescription

Every field in a case class should have an instance of `Descriptor` in order for the automatic derivation to work.
This doesn't mean the entire design of zio-config is typeclass based. For the same reason, the typeclass
`Descriptor` exists only in zio-config-magnolia (or zio-config-shapeless) package.

As an example, given below is a case class where automatic derivation won't work, and result in a compile time error:
Assume that, `AwsRegion` is a type that comes from AWS SDK.

```
  import java.time.ZonedDateTime

  case class Execution(time: AwsRegion, id: Int)
```

In this case, `descriptor[Execution]` will give us the following descriptive compile error.

```
magnolia: could not find Descriptor.Typeclass for type <outside.library.package>.AwsRegion
  in parameter 'time' of product type <your.packagename>.Execution
```

This is because zio-config-magnolia failed to derive an instance of Descriptor for AwsRegion.

In order to provide implicit instances, following choices are there

```
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

 implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
   Descriptor[String].transform(string => AwsRegion.from(string), _.toString)

```

Now `descriptor[Execution]` compiles.

### Is that the only way for custom derivation ?

What if our custom type is complex enough that, parsing from a string would actually fail?
The answer is, zio-config provides with all the functions that you need to handle errors.

```
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

  implicit val descriptorO: Descriptor[ZonedDateTime] =
    Descriptor[String].transformEitherLeft(x => Try (ZonedDateTime.parse(x)).toEither)(_.toString)(_.getMessage)
```

What is transformEitherLeft ? Parsing a String to ZonedDateTime can fail, but converting it back to a string
won't fail. Logically, these are respectively the first 2 functions that we passed to transformEitherLeft. The third
parameter talks about how to covert the error type E, which in this case is Throwable, to a proper string which is then required
for zio-config to report back to the user what's going on.

PS: We recommend not to use `throwable.getMessage`. Provide more descriptive error message.

You can also rely on `transformEither` if both `to` and `fro` can fail. This is the most commonly used combinator in zio-config.
Similarly if the error type is already String, then you can use `transformEither` and avoid having to tell the API, how to convert
`E` to `String`. We have already seen these types.

### Please give descriptions wherever possible for a better experience

Giving descriptions is going to be helpful. While all the built-in types have documentations, it is better we give
some description to custom types as well. For example:


```
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}


  implicit val awsRegionDescriptor: Descriptor[Aws.Region] =
    Descriptor[String].transform(string => AwsRegion.from(string), _.toString) ?? "value of type AWS.Region"
```

This way, when there is an error due to MissingValue, we get an error message (don't forget to use prettyPrint)
that describes about the config parameter. For example, see the `Details` corresponding to the first
`MissingValue` in a sample error message below.

```scala
 ReadError:
 ╥
 ╠─MissingValue
 ║ path: aws.region
 ║ Details: value of type AWS.Region
 ║
 ╠─FormatError
 ║ cause: Provided value is 3dollars, expecting the type double
 ║ path: cost
 ▼

```

### Where to place these implicits ?

If the types are owned by us, then the best place to keep implicit instance is the companion object of that type.

```scala

final case clas MyAwsRegion(value: AwsRegion)

object MyAwsRegion {
  implicit val awsRegionDescriptor: Descriptor[MyAwsRegion] =
    Descriptor[String]
      .transform(
        string => MyAwsRegion(AwsRegion.from(string)), 
        _.value.toString
      ) ?? "value of type AWS.Region"
}

```

However, sometimes, the types are owned by an external library.

In these situations, better off place the implicit closer to where we call the automatic derivation.
Please find the example in `magnolia` package in examples module.

### Change Keys (CamelCase, kebab-case etc)

Please find the examples in ChangeKeys.scala in magnolia module to find how to manipulate
keys in an automatic derivation such as being able to specify keys as camelCase, kebabCase or snakeCase in
the source config.