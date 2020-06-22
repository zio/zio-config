---
id: design_index
title:  "Design Principles"
---

ZIO-config is zio-native library - simply means: 
The only dependency of `zio-config` is `ZIO` , and will be using `ZIO` for any effectful computations (when reading config, integrating with other systems etc)

The library will be the fundamental part of the ZIO ecosystem, and will hopefully be the first step of any ZIO-based applications.
In nutshell, zio-config can

1. **Read** the configuration from multiple sources
2. **Write** the configuration back to various formats
3. **Document** the configuration (man page)
4. **Report** on configuration (man page, including the value obtained for each parameter)

The application config is made accessible anywhere in your app, through ZIO environment.

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
In that case all you need is just the case class `Prod` and call `descriptor[Prod]`. 
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

This is backed by significant number of examples.

## Purely Functional and Composable, yet welcoming!

zio-config is built on sound foundations, leveraging the well-known, lawful abstractions of functional programming.
ZIO Config features no implicits, no type classes, no dependencies, and no jargon!
Yet secretly it's powered by a type of applicative functor that cannot be described using standard type classes (arrows are partial isomorphisms).

A good example of accessible "stealth FP"!
