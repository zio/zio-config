---
id: automatic-validations
title: "Validation"
sidebar_label: "Validation"
---

`zio-config-refined` checks settings against predicates when they are loaded. A value that breaks a predicate fails with a `Config.Error`, the same way a missing key does. You write the rule once, in the type, and the library enforces it everywhere the type is used.

The module is built on the [refined](https://github.com/fthomas/refined) library. If you are new to refined, read its introduction first. The examples below use refined types such as `NonEmptyString` and `Refined[Int, GreaterEqual[...]]`.

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.refined._
import zio.config.magnolia.deriveConfig
```

## Two ways to write a refined setting

- **`refineType[T]("KEY")`** reads a key and checks it against a full refined type, such as `String Refined NonEmpty`.
- **`refine[P]("KEY")`** reads a key and checks it against a predicate `P`. Use it when you already have the predicate.

## Basic example

```scala mdoc:silent
import eu.timepit.refined.types.string.NonEmptyString

final case class Jdbc(username: NonEmptyString, password: NonEmptyString)

val jdbc: Config[Jdbc] =
  (refineType[NonEmptyString]("username") zip refineType[NonEmptyString]("password")).to[Jdbc]
```

Loading an empty `username` fails. The error names the key:

```scala mdoc:compile-only
val failing: IO[Config.Error, Jdbc] =
  ConfigProvider.fromMap(Map("username" -> "", "password" -> "secret")).load(jdbc)
```

## Predicates directly

Use `refine` when you want to write the predicate yourself, for example a URL:

```scala mdoc:silent
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url

val urlConfig: Config[Refined[String, Url]] = refine[Url]("URL")
```

`refine` and `refineType` return a `Config`, so they combine with `zip`, `optional` and the other functions in [Config descriptors](./config-descriptors.md).

## Validating a whole list

Refine a derived description. For example, require a list with more than two entries:

```scala mdoc:silent
import eu.timepit.refined.W
import eu.timepit.refined.collection.Size
import eu.timepit.refined.numeric.Greater
import zio.config.magnolia.deriveConfig

final case class Replica(url: String, port: Int)

val replicas: Config[List[Replica]] =
  Config.listOf("replicas", deriveConfig[Replica])

val atLeastThree: Config[Refined[List[Replica], Size[Greater[W.`2`.T]]]] =
  refine[Size[Greater[W.`2`.T]]](replicas)
```

## Validated fields in derived configs

A derived case class can have refined fields. Derivation uses the refined type's own instance:

```scala mdoc:silent
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.GreaterEqual

final case class Listener(
  host: Refined[String, NonEmpty],
  port: Refined[Int, GreaterEqual[W.`1024`.T]]
)

val listener: Config[Listener] = deriveConfig[Listener]
```

With this description, a `port` below 1024 fails to load. Combine it with `mapKey` if the keys need a different case, as in `deriveConfig[Listener].mapKey(_.toUpperCase)`.

## Next steps

- [Errors](./errors.md) explains how a failed predicate is reported.
- [Derivation](./automatic-derivation-of-config.md) explains how refined fields are derived.
