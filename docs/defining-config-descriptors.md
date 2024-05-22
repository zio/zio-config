---
id: defining-config-descriptors
title: "Defining Config Descriptors"
---

ZIO Config uses the `Config[A]` to describe the configuration of type `A`, which is part of ZIO core library. So before diving into ZIO Config, we need to understand the `Config[A]` data type. There is a [dedicated section](https://zio.dev/reference/configuration/) in the ZIO documentation that explains what are config descriptors and how we can create them.

There are two ways to create ZIO config descriptors:
1. **Describing Configuration Descriptors Manually** — We can manually create a configuration descriptor using the `Config` data type and its compositional operators.
2. **Auto-derivation of Configuration Descriptors** — We can derive a configuration descriptor for a case class or sealed trait using the `zio-config-magnolia` module.

Let's talk about both of these methods in detail.

## Describing Configuration Descriptors Manually

We must fetch the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala mdoc:silent
case class MyConfig(ldap: String, port: Int, dburl: String)
```

To perform any action using ZIO Config, we need a configuration description. Let's define a simple one. To generate a `Config[MyConfig]` we can first generate tuples of the primitive configurations like `string`, `int`, etc using the `zip` operator, then map them to their respective case class:

```scala mdoc:silent
import zio._
import zio.config._
import zio.ConfigProvider
import zio.Config, Config._

object MyConfig {
  val config: Config[MyConfig] = (string("LDAP") zip int("PORT") zip string("DB_URL")).to[MyConfig]
}
```

There are several other combinators which can be used to describe the configuration. To learn more please refer to the ZIO core reference section for [configuration](https://zio.dev/reference/configuration/).

## Auto-derivation of Config Descriptors

If we don't like describing our configuration manually, we can use the `zio-config-magnolia` module to derive the configuration descriptor for a case class or a sealed trait. Let's add this module to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

By importing the `zio.config.magnolia._` package, we can derive the configuration descriptor for a case class or a sealed trait using the `deriveConfig` method:

```scala mdoc:silent:nest
import zio.config._
import zio.config.magnolia._

case class MyConfig(ldap: String, port: Int, dburl: String)

object MyConfig {
  implicit val config: Config[MyConfig] = deriveConfig[MyConfig]
}
```

## Read config from various sources

There are more information on various sources in [here](read-from-various-sources.md).

Below given is a simple example.

```scala mdoc:silent
val map =
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

val source = ConfigProvider.fromMap(map)

source.load(MyConfig.config)
```
