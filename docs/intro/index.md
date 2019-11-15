---
id: intro_index
title:  "Introduction"
---

**zio-config** 

ZIO Config aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application.

Anything such as configuration management should be the most easiest thing to do, hence we aim
at zero boilerplates and complexities and make it insanely simple to use.

### Notable features    

zio-config exposes an intuitive language, that allows you to describe your configuration.
We will be using the term "configuration description" quite often acrosss the documentation

The notable features of zio-config include

* Composable and easy-to-write configuration description - that can read configuration from various sources
* Support for writing the configuration back (to the application environments) from a typesafe representation, given the same configuration description.
* Support for giving more details (as documentation) for each configuration parameter as part of configuration description
* Auto generation of documentation for the entire config (sort of a manpage) based on the description
* It can also emit a report on the configuration values, as part of the same documentation
* Handles nested configuration structures
* Handles multiple (composable) sources (setting the priority of sources using simple combinators, resetting the sources etc)
* Accumulates all errors rather than bailing at the first while reading the config from these sources
* Zero implicits
* Zero macros
* Zero higher kinded types.
* The one and only dependency is zio
* Simple altogether!

While this is too much as a list, this is backed by significant number of examples, which enables you to make the full use of zio-config.

In short, this is your go-to library for all your configuration management requirements, especially if you are in the zio world!
We are keen to get new feature requests, issues, suggestions and contributions into this library

# For impatients

If you are impatient, trying to understand everything zio-config can do as actual code, please head to examples project.
One of the example is an entire zio application that makes use of zio-config to read the config,
and make it available across the application as just another zio [Environment](https://zio.dev/docs/overview/overview_index#zio).

# The simplest example

Otherwise, we will go a bit slowly, especially the principles behind this library
can be quite interesting to you, which you can adopt to your other usecases.

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
