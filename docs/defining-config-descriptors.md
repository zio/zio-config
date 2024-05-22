---
id: defining-config-descriptors
title: "Defining Config Descriptors"
---

ZIO Config uses the `Config[A]` to describe the configuration of type `A`, which is part of ZIO core library. So before diving into ZIO Config, we need to understand the `Config[A]` data type. There is a [dedicated section](https://zio.dev/reference/configuration/) in the ZIO documentation that explains what are config descriptors and how we can create them.

## Defining Config Descriptors

There are two ways to create ZIO config descriptors:
1. **Manual Definition of Configuration Descriptors** — We can manually create a configuration descriptor using the `Config` data type and its compositional operators.
2. **Auto-derivation of Configuration Descriptors** — We can derive a configuration descriptor for a case class or sealed trait using the `zio-config-magnolia` module.

Let's talk about both of these methods in detail.

### Manual Definition of Config Descriptors

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

### Auto-derivation of Config Descriptors

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

## Accumulating all errors

For any misconfiguration, the `ReadError` collects all of them with proper semantics: `AndErrors` and `OrErrors`.
Instead of directly printing misconfigurations, the `ReadError.prettyPrint` shows the path, detail of collected misconfigurations.

1. All misconfigurations of `AndErrors` are put in parallel lines.

```text
╥
╠══╗ 
║  ║ FormatError
║ MissingValue
``` 

2. `OrErrors` are in the same line which indicates a sequential misconfiguration

```text
╥
╠MissingValue
║
╠FormatError
```

Here is a complete example:

```text
   ReadError:
   ╥
   ╠══╦══╗
   ║  ║  ║
   ║  ║  ╠─MissingValue
   ║  ║  ║ path: var2
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ╠─MissingValue path: envvar3
   ║  ║  ║ path: var3
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ▼
   ║  ║
   ║  ╠─FormatError
   ║  ║ cause: Provided value is wrong, expecting the type int
   ║  ║ path: var1
   ║  ▼
   ▼
```

## Operations

### Mapping keys

Now on, the only way to change keys is as follows:

```scala
  // mapKey is just a function in `Config` that pre-existed

  val config = deriveConfig[Config].mapKey(_.toUpperCase)
```

### CollectAll

```scala mdoc:compile-only
import zio._
import zio.config._

  final case class Variables(variable1: Int, variable2: Option[Int])

  val listOfConfig: List[Config[Variables]] =
    List("GROUP1", "GROUP2", "GROUP3", "GROUP4")
      .map(group => (Config.int(s"${group}_VARIABLE1") zip Config.int(s"${group}_VARIABLE2").optional).to[Variables])

  val configOfList: Config[List[Variables]] =
    Config.collectAll(listOfConfig.head, listOfConfig.tail: _*)
```

### orElseEither && Constant

```scala mdoc:compile-only
import zio._
import zio.config._ 

sealed trait Greeting

case object Hello extends Greeting
case object Bye extends Greeting

val configSource = 
  ConfigProvider.fromMap(Map("greeting" -> "Hello"))

val config: Config[String] = 
  Config.constant("Hello").orElseEither(Config.constant("Bye")).map(_.merge)
```
