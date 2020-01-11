---
id: refined_index
title:  "Automatic Validations"
---

By bringing in `zio-config-refined` module, you get validations for your config parameters almost for free. 
`zio-config` elegantly integrates with `Refined` library for you to achieve this with same ergnomics!

```scala mdoc:silent

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import zio.config.ConfigDescriptor._
import zio.config._
import zio.config.refined._
import zio.{ App, ZEnv, ZIO }

case class RefinedProd(
  ldap: Refined[String, NonEmpty],
  port: Refined[Int, GreaterEqual[W.`1024`.T]],
  dburl: Option[Refined[String, NonEmpty]],
  longs: Refined[List[Long], Size[Greater[W.`2`.T]]]
)

def prodConfig: ConfigDescriptor[String, String, RefinedProd] =
  (
    nonEmpty(string("LDAP")) |@|
      greaterEqual[W.`1024`.T](int("PORT")) |@|
      nonEmpty(string("DB_URL")).optional |@|
      size[Greater[W.`2`.T]](list(long("LONGVALS")))
  )(
    RefinedProd.apply,
    RefinedProd.unapply
  )

val myAppLogic: ZIO[RefinedProd, Nothing, Refined[List[Long], Size[Greater[W.`2`.T]]]] =
  ZIO.access[RefinedProd](_.longs)

val configMultiMap =
  Map(
    "LDAP"     -> singleton("ldap"),
    "PORT"     -> singleton("1999"),
    "DB_URL"   -> singleton("ddd"),
    "LONGVALS" -> ::("1234", List("2345", "3456"))
  )
  
val outcome: ZIO[Any, ReadErrors[Vector[String], String], Refined[List[Long], Size[Greater[W.`2`.T]]]] =
  for {
    config <- read(prodConfig.from(ConfigSource.fromMultiMap(configMultiMap)))
    r      <- myAppLogic.provide(config)
  } yield r
```

Check out sample usage of `zio-config-refined` in `examples` module of the project.