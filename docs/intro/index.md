---
id: intro_index
title:  "Introduction"
---

ZIO Config aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application.

We aim at zero complexity and make it insanely simple to use to manage configurations.
With this library, we also put forward the notion that we can do FP in scala, without too much relatively horrifying complexities.

In nutshell, zio-config can

1. **Read** the configuration from multiple sources
2. **Write** the configuration back to various formats
3. **Document** the configuration (man page)
4. **Report** on configuration (man page, including the value obtained for each parameter)

The application config is made accessible anywhere in your app, through ZIO environment.
And for more reasons, this will be your go-to library for all your configuration management requirements, if you are in the zio world

## Design 

The main design choice that we made is, describe the configuration (manually, or automated) once and for all, and use the description for various actions.
Hence, zio-config exposes an intuitive language, that allows you to describe your configuration.

```scala
  case class Prod(ldap: String, port: Int, dburl: Option[String])

  val prodConfig =
    (string("LDAP") |@| int("PORT") |@|
      string("DB_URL").optional)(Prod.apply, Prod.unapply)

```

We do have support for **_automatic configuration description through zio-config-magnolia_**, if you consider this description as a boiler-plate.
In that case all you need is just the case class `Prod` and call `description[Prod]`. 
More on this later.

_Note_ : We will be using the term "configuration description" quite often across the documentation

### Zero boiler plate
With zero boiler-plate, we mean, no implicits and no typeclass syntax imports as we see in almost all purely functional configuration libraries.
All you need is a description and zio-config can do various actions with this description.

### We still support Magic
As mentioned before, in situations where you need a fully automated description, then we have zio-config-magnolia for you.

### Zero dependency
zio-config stays zero dependent from core functional libraries in scala, and hence don't intercept your choice.

## Notable features

* **Describe once and use it for different actions:**
  - Read configuration from various sources.
  - Write the configuration back, given the same description and a value of the config.
  - Document the configuration (or support page / help-page), mainly useful for ops team.
  - Report the application configuration, providing the value of each parameter alongside documentation.
* Composable interface to give extra documentation for each config parameter, and in fact for a whole configuration.
* Accumulates all errors rather than bailing at the first.
* Handles nested configuration.
* Composable multiple sources.
* Set priority for sources.
* Reset sources.
* Support for `Refined` types.
* Support for automatic configuration description
* Integration with zio environment.
* Zero implicits
* Zero macros
* The one and only dependency is zio, because we stick on to ZIO as the functional effect.
* Simple altogether!

This is backed by significant number of examples/

## For impatients

To try out zio-conifg straight away, please head on to [examples](https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples) project.
One of the example is an entire zio application demonstrates the use of zio-config to read the config,
and make it available across the application as just another zio [Environment](https://zio.dev/docs/overview/overview_index#zio).

Otherwise, `Read`, `Write`, `Documentation` and `Reporting` are separately documented.

## Principles

### ZIO-Native

ZIO-config is zio-native library - simply means, we use zio for any effects inside, and provides integration with ZIO Environment to access your application config.

ZIO-Config will be the fundamental part of the ZIO ecosystem, and will hopefully be the first step of any ZIO-based applications.

## Purely Functional and Composable

zio-config is built on sound foundations, leveraging the well-known, lawful abstractions of functional programming.
Principal abstractions that inform the zio-config implementation are:

#### Free Applicative, in simple scala with zero sophistications

  Everything library does inside, is by making use of program introspection - a feature that goes well with a Free Applicative style encoding. However, this doesn't make it any inacessible to those who are unfamiliar with the principle, and that's because we made it possible with simple scala with zero sophistications. We made it as minimal as possible - for contributors and users of the library.

#### And Invariant applicative in nature

There are multiple places of bidirectionality in this library. Hence, there is a behavior of invariant applicative functor lying with the Free Applicative style encoding. 
We are not coupled to any specific typeclass instances, enabling us to scalably encode more functionalities, and features while strongly adhering to laws and FP fundamentals. 

For the same reason, orthogonality across the functionalities came long in a natural way, without having to fiddle with sophistications and implicit instances. For instance, we added the fact that writing the config can fail as well, in a few minutes - scalably, in an aesthetically pleasing manner! This was possible as we are neither going into over generalisations nor into over restrictions.
