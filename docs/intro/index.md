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
* Support for writing the configuration back to the application environment, given the same configuration description.
* Support for documenting configuration parameter as part of configuration description
* Auto generation of documentation of application configuration
* It can also emit a report on the configuration values, as part of the same documentation
* Handles nested configuration structures
* Handles multiple (composable) sources (such as setting the priority of sources using simple combinators, resetting the sources etc)
* Accumulates all errors rather than bailing at the first while reading the config from these sources
* Zero implicits
* Zero macros
* Zero higher kinded types.
* The one and only dependency is zio
* Simple altogether!

This is backed by significant number of examples, which enables you to make the full use of zio-config.

In short, this is your go-to library for all your configuration management requirements, especially if you are in the zio world!
We are keen to get new feature requests, issues, suggestions and contributions into this library

# For impatients

If you ar trying to understand everything zio-config can do as actual code, please head to examples project.
One of the example is an entire zio application demonstrates the use of zio-config to read the config,
and make it available across the application as just another zio [Environment](https://zio.dev/docs/overview/overview_index#zio).

The principles of functional programming behind this library
can be quite interesting to you, which you can adopt to your other usecases. 
That said, we believe in minimum dependencies - in fact, zero dependencies.

# The simplest example

 To just read the config, all you need is;

```scala

  import zio.config._, ConfigDescriptor._

  case class Prod(ldap: String, port: Int, dburl: Option[String])

  val prodConfig =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

  // Supports sys env, sys properties (with the support of nested ones - hoccon support is on its way)
  val source =
    ConfigSource.fromMap(
      Map(
        "LDAP"             -> "abc",
        "DB_URL"           -> "some.url",
        "PORT"             -> "8888",
      )
    )

  val cfg: IO[ConfigErrors[String, String], AwsConfig] = 
    Config.fromEnv(prodConfig)
    // You can also do read(prodConfig from ConfigSource.fromEnv)
```

To write the config back. 

You can use the configuration description to write back the config, may be to a different source. Writing resulting in a PropertyTree that holds the structural
essence of the configuration description which you easily emit to the source (environment) that you would like to have.

By that way, you just migrated your application config from being in sys.env / property-file (where nestedness is (horribly) represented by underscores and dots) to a much more
application specific hoccon file in the most type safe way, for instance. 

You will also use write in your test cases which allows you to test your application from ground up, instead of bypassing the configuration parsing.
You can also use the same to dynamically update the environment that holds the configuration, while making sure that application will definitely read it! 

```scala
  val propertyTree = write(prodConfig, result)
```

To document the config

```scala
  val result: ConfigDocs = docs(prodConfig) 
  // docs(prodConfig from ConfigSource.fromEnv)) to doc about the source as well

  // Produce docs, spewing out the values help by each parameter;
  val awsConfig: AwsConfig = ??? // which you can get by read(prodConfig from ConfigSource.fromEnv)
  val result: ConfigDocs = docs(prodConfig, awsConfig)

```

Head to examples project to get more insights. The docs will cover an explanation of each example

# Principles

## ZIO-Native

zio-config is a zero-dependency, ZIO-native library.
It is a fundamental part of the ZIO ecosystem, and a building block for the creation of ZIO-based applications.

## Purely Functional and Composable

zio-config is built on sound foundations, leveraging the well-known, lawful abstractions of functional programming.
Principal abstractions that inform the zio-config implementation are:

* Free Applicative, in simple scala with zero sophistications
  Everything this library does inside, is by making use of program introspection - a feature that goes well with a Free Applicative encoding. This is made possible by sticking
  on to simple scala, making it minimal in nature - for contributors and users of the library.

* And Invariant applicative in nature

There are multiple places of bidirectionality in this application. Hence, there is a behavior of invariant applicative functor lying in the Free Applicative encoding. 
However we are not coupled to any specific typeclass instances, enabling us to scalably encode more functionalities, and features while strongly adhering to laws and FP fundamentals. 

This also allowed us to adhere to orthogonality principle in a natural way, without having to fiddle with sophistications and implicit instances. For example, towards the first release we added the fact that writing the config can fail as well, and this change in behavior was done in a few minutes - scalably, in an aesthetically pleasing manner! We are neither going into over generalisations nor into over restrictions.
