Z---
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

Also, there are a few differences when it comes to scala-3 for auto-matic derivation.
Refer scala-3 section in this page for specific differences. Most of them, 
is an intentional reduction in the number of moving parts in the entire mechanism of auto-derivation, plus,
a few subtle limitations.

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
  import zio.config.magnolia._

  import X._
```

```scala

  // Defining different possibility of HOCON source

  val aHoconSource =
    ConfigSource
      .fromHoconString("x = A")
```

```scala
  val bHoconSource =
    ConfigSource
      .fromHoconString("x = B")
```

```scala
  val cHoconSource =
    ConfigSource
      .fromHoconString("x = C")
```

```scala
  val dHoconSource =
    ConfigSource
      .fromHoconString(
        s"""
           | x {
           |   DetailsWrapped {
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

To know more about various semantics of `descriptor`, please refer to the [api docs](https://javadoc.io/static/dev.zio/zio-config-magnolia_2.13/1.0.0-RC31-1/zio/config/magnolia/index.html#descriptor[A](implicitconfig:zio.config.magnolia.package.Descriptor[A]):zio.config.ConfigDescriptor[A]).

**NOTE**


The fieldNames and classnames remain the same as that of case-classes and sealed-traits.

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
   (string("region") zip string("dburl").transform(DbUrl, _.value)).to[Aws] ?? "This config is about aws"
```

You could provide `describe` annotation at field level

Example:

```scala
case class Aws(@describe("AWS region") region: String, dburl: DbUrl)
```

This will be equivalent to the manual configuration of:


```scala
   (string("region") ?? "AWS region" zip string("dburl").to[DbUrl]).to[Aws] ?? "This config is about aws"
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

Custom descriptors are also needed in case you use value classes to describe your configuration. You can use them
together with automatic derivation and those implicit custom descriptors will be taken automatically into account

```scala
import zio.config.magnolia.{descriptor, Descriptor}

final case class AwsRegion(value: String) extends AnyVal {
  override def toString: String = value
}

object AwsRegion {
  implicit val descriptor: Descriptor[AwsRegion] = 
    Descriptor[String].transform(AwsRegion(_), _.value)
}
```

### Is that the only way for custom derivation ?

What if our custom type is complex enough that, parsing from a string would actually fail?
The answer is, zio-config provides with all the functions that you need to handle errors.

```
 import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor}

  implicit val descriptorO: Descriptor[ZonedDateTime] =
    Descriptor[String].transformOrFailLeft(x => Try (ZonedDateTime.parse(x)).toEither.swap.map(_.getMessage).swap)(_.toString)
```

What is transformOrFailLeft ? Parsing a String to ZonedDateTime can fail, but converting it back to a string
won't fail. Logically, these are respectively the first 2 functions that we passed to transformEitherLeft. 

PS: We recommend not to use `throwable.getMessage`. Provide more descriptive error message.

You can also rely on `transformOrfail` if both `to` and `fro` can fail. 

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


## Scala3 Autoderivation
Works just like scala-2.12 and scala-2.13, however we don't have DeriveConfigDescriptor anymore.
If possible, we will make this behaviour consistent in scala-2.12 and scala-2.13 in future versions of zio-config.

#### No more DeriveConfigDescriptor class

If you are in scala-3, just make sure you **don't** have the import
`zio.config.magnolia.DeriveConfigDescriptor.descriptor`, because  `DeriveConfigDescriptor`
doesn't exist in scala3. 

Instead, use `zio.config.magnolia.descriptor`. This already works in scala-2.x as well.

Eventually, we hope to bring the same behaviour in scala-2.x and make it consistent across
all versions.

### No support for `AnyVal` 

You may encounter the following error, if you have `AnyVal` in your config.

```scala

no implicit argument of type zio.config.magnolia.Descriptor

```

If looking for new-types, use better strategies than AnyVal (https://afsal-taj06.medium.com/newtype-is-new-now-63f1b632429d),
and add custom `Descriptor`explicitly in its companion objects.

We will consider adding `AnyVal` support, for supporting legacy applications in future versions
of zio-config.

In the meantime, if you are migrating from Scala 2 where you had custom descriptors defined for value classes
you need to slightly change your code to compile it with Scala 3
 - remove `AnyVal` trait
 - modify definition of custom descriptor
 - add import to include `ConfigDescriptor` combinators

```scala
import zio.config.ConfigDescriptor._
import zio.config.magnolia.{descriptor, Descriptor}

final case class AwsRegion(value: String) {
  override def toString: String = value
}

object AwsRegion {
  given Descriptor[AwsRegion] =
    Descriptor.from(string.to[AwsRegion])
}
```
this way there is no need for you to update the configuration files.

### No more SealedStraitStrategy

If you are familiar with `SealedTraitStrategy` in zio-config for scala-2.x, you will miss it in scala-3. 
With scala-3 (and hopefully in scala-2.x in future versions) most of the outcomes that you
get using `SealedStraitStrategy` is possible with `name` (or `names`) annotations. 

We think, this is more explicit, and less of a magic compared to creating `customDerivation` by 
extending `DeriveConfigDescriptor`

#### Example:

The name of the sealed trait itself is skipped completely by default.
However, if you put a `name` annotation on top of the sealed-trait itself,
then it becomes part of the config. 

The name of the case-class should be available in config-source, 
and by default it should the same name as that of the case-class.

```scala

sealed trait A
case class B(x: String) extends A
case class C(y: String) extends A

case class Config(a: A) 

```

With the above config, `descriptor[A]` can read following source.

```scala

 {
   "a" : {
    "B" : {
       "x" : "abc"
      }
    }
   }
 }

// or a.B.x="abc", if your source is just property file
```

However, if you give `name` annotation for A, the name of the sealed trait
should be part of the config too. This is rarely used.

```scala

@name("aaaaa")
sealed trait A
case class B(x: String) extends A
case class C(y: String) extends A

case class Config(a: A) 

```

With the above config, `descriptor[A]` can read following source.

```scala


 {
   "a" : {
     "aaaaa" : 
       "B" : {
         "x" : "abc"
      }
    }
   }
 }



```
Similar to scala-2.x, you can give name annotations to any case-class as well (similar to scala 2.x)

#### Example:

```scala

sealed trait A

@name("b")
case class B(x: String) extends A
case class C(y: String) extends A

case class Config(a: A) 


```

In this case, the config should be

```scala

 {
   "a" : {
    "b" : {
       "x" : "abc"
      }
    }
   }
 }

```

### No guaranteed behavior for scala-3 enum yet
With the current release, there is no guaranteed support of scala-3 enum. 
Use `sealed trait` and `case class` pattern.

### No support for recursive config in auto-derivation, but we can make it work

There is no support for auto-deriving recursive config with scala-3.
Please refer to examples in magnolia package (not in the main examples module)
