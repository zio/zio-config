---
id: intro_index
title:  "Introduction"
---

**zio-config** provides one-stop capabilities for application configuration. Notable features:

* Composable syntax
* Zero dependency
* Simple API with no implicits or macro magic
* Auto-generates documentation
* Can write the config back, allowing generation of valid configuration sets from in-memory configuration objects
* Automatic report generation for configuration value lineage
* Handles nested configuration structures
* Accumulates all errors rather than bailing at the first
* Insanely simple to use

# Motivating Example

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(db1: Database, db2: Database, appName: String)

  val database: ConfigDescriptor[Database] =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig: ConfigDescriptor[AwsConfig] =
    (nested("south") { database } ? "South region details" |@|
      nested("east") { database } ? "East region details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

  val source: ConfigSource[String, String] =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )

  val cfg: IO[ReadErrors[String, String], AwsConfig] = 
    read(appConfig).provide(source)
```

This sample will yield the configured value `AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp"))`.

There is a lot going on here.

* The configuration data structures are declared in an applicative combination style,
using the familiar `|@|` operator.
* Configuration is a two-way process, covering:
  * Reading from some configuration source such as a `Map` or, more likely, system environment variables.
  * Writing back to a configuration target.
* The two-way configuration process is described by the invariant functor-style functions `(Database.apply, Database.unapply)`.
* Nested configuration classes are supported via the `nested` keyword, for example `nested("south") { database }`
* The definition includes documentation via the `?` operator, for example `? "South region details"`
* This documentation can be used at runtime to auto-generate a configuration manual 
* `ConfigSource` describes where the configuration values come from. In this simple example, they come from a in-memory `Map[String, String]` 
* zio-config is able to reuse the `Database` definition nested within `AwsConfig`.

# Principles

## ZIO-Native

zio-config is a zero-dependency, ZIO-native library.
It is a fundamental part of the ZIO ecosystem, and a building block for the creation of ZIO-based applications.

## Purely Functional

zio-config is built on sound foundations, leveraging the well-known, lawful abstractions of functional programming.
Principal abstractions that inform the zio-config implementation are:

* Applicative
  * Configuration elements are composed together in an applicative style
  * Configuration errors are accumulated using applicative combination – this means all errors are gathered and reported together
* Invariant Functor
  * zio-config implements a more-specialised form of an invariant functor, which is an abstraction that describes a *codec* – something that provides a two-way encoding / decoding. 
    In this case, *encoding* is configuration *writing* while *decoding* is the more common configuration *reading*. 

## Composable

Using the applicative `|@|` syntax, zio-config allows users to build up descriptions of configuration structures
of arbitrary depth and complexity.
This divide-and-conquer approach keeps things simple and extensible.
