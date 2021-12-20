---
id: refined_index
title:  "Automatic Validations"
---

By bringing in `zio-config-refined` module, you get validations for your config parameters almost for free. 
`zio-config` elegantly integrates with `Refined` library for you to achieve this with same ergnomics.

If you are not familiar with `refined` library, refer https://github.com/fthomas/refined.

There are various ways that zio-config can interact with refined library. 
Take a look at `zio.config.refined` package.

```scala mdoc:silent
 import zio.config._, refined._

```

A few examples are given below.

## Basic Example

```scala mdoc:silent
 import eu.timepit.refined.types.string.NonEmptyString

 case class Jdbc(username: NonEmptyString, password: NonEmptyString)

 val jdbc: ConfigDescriptor[Jdbc] =
   (refineType[NonEmptyString]("username") zip
     refineType[NonEmptyString]("password")).to[Jdbc]

 read(jdbc from ConfigSource.fromMap(Map("username" -> "", "password" -> "")))

```

## Direct Interaction with Refined Predicates

If you need to directly interact with `Predicate`s (ex: `NonEmpty`), then
`refine[A, P]` method is useful.

```scala mdoc:silent
 import eu.timepit.refined._, api._, string._

 val urlConfig: ConfigDescriptor[Refined[String, Url]] =
   refine[String, Url]("URL")

```

## Derive from existing ConfigDescriptor

Of various methods available in `zio.config.refined` package, 
the most interesting one is being able to get a refined type out of an already derived ConfigDescriptor.
This shows the composable nature of zio-config. 

Take a look at the below example

```scala mdoc:silent
 import zio.config.magnolia.descriptor

 import eu.timepit.refined._, api._, numeric._, collection._
 import ConfigDescriptor.list

 case class MyConfig(url: String, port: Int)

 val configs: ConfigDescriptor[List[MyConfig]] =
   list("databases")(descriptor[MyConfig])

 // A list of database configs, such that size should be greater than 2.
 val databaseList: ConfigDescriptor[Refined[List[MyConfig], Size[Greater[W.`2`.T]]]] =
   refine[Size[Greater[W.`2`.T]]](configs)
```

## Auto-Derivation and Refined

You can also use auto derivations with refined.

```scala mdoc:silent
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.{ NonEmpty, Size }
import zio.config.magnolia.descriptor

object RefinedReadConfig extends App {
  case class RefinedProd(
    ldap: Refined[String, NonEmpty],
    port: Refined[Int, GreaterEqual[W.`1024`.T]],
    dbUrl: Option[Refined[String, NonEmpty]],
    longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
  )

  val configMultiMap =
    Map(
      "LDAP"     -> ::("ldap", Nil),
      "PORT"     -> ::("1999", Nil),
      "DBURL"   -> ::("ddd", Nil),
      "LONGS" -> ::("1234", List("2345", "3456"))
    )

  val result =
    read(descriptor[RefinedProd].mapKey(_.toUpperCase) from ConfigSource.fromMultiMap(configMultiMap))

  // Right(RefinedProd(ldap,1999,Some(ddd),List(1234, 2345, 3456)))
}

```
