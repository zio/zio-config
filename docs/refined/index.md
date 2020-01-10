---
id: refined_index
title:  "Automatic Validations"
---

By bringing in `zio-config-refined` module, you get validations for your config parameters almost for free. 
`zio-config` elegantly integrates with `Refined` library for you to ahieve this with same ergnomics!

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

def prodConfig(n: Int): ConfigDescriptor[String, String, RefinedProd] =
  (
    nonEmpty(string("LDAP")) |@|
      greaterEqual[W.`1024`.T](int("PORT")) |@|
      nonEmpty(string("DB_URL")).optional |@|
      size[Greater[W.`2`.T]](longs(n).xmap(_.toList)(list => ::(list.head, list.tail)))
  )(
    RefinedProd.apply,
    RefinedProd.unapply
  )

def longList(n: Int): List[ConfigDescriptor[String, String, Long]] =
  (1 to n).toList
    .map(group => long(s"GROUP${group}_LONGVAL"))

def longs(n: Int): ConfigDescriptor[String, String, ::[Long]] =
  ConfigDescriptor.collectAll[String, String, Long](::(longList(n).head, longList(n).tail))

val myAppLogic: ZIO[Config[RefinedProd], Nothing, Refined[List[Long], Size[Greater[W.`2`.T]]]] =
  for {
    prod <- config[RefinedProd]
  } yield prod.longs

val configMap =
  Map(
    "LDAP"           -> "ldap",
    "PORT"           -> "1999",
    "DB_URL"         -> "ddd",
    "COUNT"          -> "3",
    "GROUP1_LONGVAL" -> "1234",
    "GROUP2_LONGVAL" -> "2345",
    "GROUP3_LONGVAL" -> "3456"
  )
  
val outcome =
  for {
    count  <- Config.fromMap(configMap, nonNegative(int("COUNT")))
    n      <- count.config.config
    config <- Config.fromMap(configMap, prodConfig(n.value))
    r      <- myAppLogic.provide(config)
  } yield r
  

```

Checkout examples for usage of `zio-config-refined` in examples module of the project.