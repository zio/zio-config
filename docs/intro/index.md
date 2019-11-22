---
id: intro_index
title:  "Introduction"
---

**zio-config** 

ZIO Config aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application.

We aim at zero boilerplates and complexities and make it insanely simple to use to manage configurations.

In nutshell, zio-config can

1. Read the configuration
2. Writ back the configuration
3. Document the configuration
4. Report on the configuration

The application config is made accessible anywhere in your app, through ZIO environment.
And for more reasons, this will be your go-to library for all your configuration management requirements, if you are in the zio world

### Notable features    

zio-config exposes an intuitive language, that allows you to describe your configuration.

_Note_ : We will be using the term "configuration description" quite often acrosss the documentation

```scala
  case class Prod(ldap: String, port: Int, dburl: Option[String])

  val prodConfig =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

```

The notable features of zio-config include

* Simple and composable configuration description - that can read configuration from various sources.
* Write the configuration back to the application environment, given the same configuration description.
* Composable syntax for describing (documenting) each configuration parameter in the description.
* Auto generation of documentation (or support page) of application configuration, mainly useful for ops team.
* Emit a report on the configuration values, providing the value of each configuration when producing the documentation.
* Handles nested configuration.
* Handles multiple (composable) sources.
* We can set priority for the sources / Reset all the sources etc.
* Configuration will be part of your environment with the use of the library.
* Accumulates all errors rather than bailing at the first while reading the config from these sources
* Zero implicits
* Zero macros
* The one and only dependency is zio
* Simple altogether!

This is backed by significant number of examples/

# For impatients

To try out straight away, please head on to examples project.
One of the example is an entire zio application demonstrates the use of zio-config to read the config,
and make it available across the application as just another zio [Environment](https://zio.dev/docs/overview/overview_index#zio).

Otherwise, `Read`, `Write`, `Documentation` and `Reporting` are separately documented.


# Principles

## ZIO-Native

zio-config is a zero-dependency, ZIO-native library.
It is a fundamental part of the ZIO ecosystem, and a building block for the creation of ZIO-based applications.

## Purely Functional and Composable

zio-config is built on sound foundations, leveraging the well-known, lawful abstractions of functional programming.
Principal abstractions that inform the zio-config implementation are:

#### Free Applicative, in simple scala with zero sophistications

  Everything library does inside, is by making use of program introspection - a feature that goes well with a Free Applicative style encoding. However, this doesn't make it any inacessible to those who are unfamiliar with the principle, and that's because we made it possible with simple scala with zero sophistications. We made it as minimal as possible - for contributors and users of the library.

#### And Invariant applicative in nature

There are multiple places of bidirectionality in this library. Hence, there is a behavior of invariant applicative functor lying with the Free Applicative style encoding. 
We are not coupled to any specific typeclass instances, enabling us to scalably encode more functionalities, and features while strongly adhering to laws and FP fundamentals. 

For the same reason, orthogonality across the functionalities came long in a natural way, without having to fiddle with sophistications and implicit instances. For instance, we added the fact that writing the config can fail as well, in a few minutes - scalably, in an aesthetically pleasing manner! This was possible as we are neither going into over generalisations nor into over restrictions.
