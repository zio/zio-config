---
id: automatic-validations
title: "Validation"
sidebar_label: "Validation"
---

`zio-config-refined` checks settings against predicates when they are loaded. A value that breaks a predicate fails with a `Config.Error`, the same way a missing key does. You write the rule once, in the type, and the library enforces it everywhere the type is used.

The module is built on the [refined](https://github.com/fthomas/refined) library. If you are new to refined, read its introduction first. For a check that doesn't need a refined type, `Config#validate` from ZIO is enough; see [Config descriptors](./config-descriptors.md).

To add the module:

```scala
libraryDependencies += "dev.zio" %% "zio-config-refined" % "@VERSION@"
```

The examples on this page share these imports and a helper, `show`, which loads a description from a map:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.refined._
import zio.config.magnolia.deriveConfig

import eu.timepit.refined.api.Refined
import eu.timepit.refined.W

def show[A](config: Config[A], entries: (String, String)*): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromMap(entries.toMap).load(config).either).getOrThrow()
  }
```

## Three Ways to Write a Refined Setting

`zio.config.refined._` has three functions. Pick the one that matches what you already have:

| Function                          | Takes                                       | Use it when                                                      |
| --------------------------------- | ------------------------------------------- | ---------------------------------------------------------------- |
| `refineType[R](key)`              | A full refined type `R`, such as `NonEmptyString` | You have a type alias for the refined type                   |
| `refine[A, P](key)`               | The value type `A` and the predicate `P`    | You want to name the predicate and the type separately           |
| `refine[P](description)`          | A predicate `P` and an existing `Config[A]` | The value already has a description, derived or hand-written     |

The first two read the key with the `DeriveConfig` instance of the value type, so they work for every type listed in the [DeriveConfig reference](./derive-config.md). All three return a `Config`, so they combine with `Config#zip`, `Config#optional` and the other functions.

### A Full Refined Type

`refineType` takes the refined type as a whole:

```scala mdoc:silent
import eu.timepit.refined.types.string.NonEmptyString

final case class Jdbc(username: NonEmptyString, password: NonEmptyString)

val jdbc: Config[Jdbc] =
  (refineType[NonEmptyString]("username") zip refineType[NonEmptyString]("password")).to[Jdbc]
```

An empty `username` fails with an `InvalidData` error that carries refined's message for the predicate:

```scala mdoc
show(jdbc, "username" -> "", "password" -> "secret")
```

:::note
The errors from `refineType` and `refine[A, P](key)` currently have an empty `path`, so a report doesn't say which key failed. A refined field in a derived case class, shown below, keeps the path. To keep the path with a hand-written description, refine the value first and add the key afterwards: `refine[NonEmpty](Config.string).nested("username")`.
:::

With the key added after the predicate, the error names `username`:

```scala mdoc
import eu.timepit.refined.collection.NonEmpty

show(refine[NonEmpty](Config.string).nested("username"), "username" -> "")
```

### A Value Type and a Predicate

`refine[A, P](key)` reads `key` as an `A` and checks it against `P`:

```scala mdoc:silent
import eu.timepit.refined.numeric.GreaterEqual

val port: Config[Refined[Int, GreaterEqual[W.`1024`.T]]] =
  refine[Int, GreaterEqual[W.`1024`.T]]("PORT")
```

A port below 1024 fails in the same way:

```scala mdoc
show(port, "PORT" -> "80")
```

### A Predicate on an Existing Description

`refine[P](description)` adds a predicate to any description. For example, to require a list with more than two entries:

```scala mdoc:silent
import eu.timepit.refined.collection.Size
import eu.timepit.refined.numeric.Greater

final case class Replica(url: String, port: Int)

val replicas: Config[List[Replica]] =
  Config.listOf("replicas", deriveConfig[Replica])

val atLeastThree: Config[Refined[List[Replica], Size[Greater[W.`2`.T]]]] =
  refine[Size[Greater[W.`2`.T]]](replicas)
```

With only one replica, the size check fails:

```scala mdoc
show(atLeastThree, "replicas[0].url" -> "a", "replicas[0].port" -> "1")
```

## Refined Fields in Derived Configs

With `zio.config.refined._` in scope, derivation has an instance for `Refined[A, P]` whenever `A` has one, so a case class can have refined fields:

```scala mdoc:silent
final case class Listener(
  host: Refined[String, NonEmpty],
  port: Refined[Int, GreaterEqual[W.`1024`.T]]
)

val listener: Config[Listener] = deriveConfig[Listener]
```

A `port` below 1024 fails to load. Here the error also carries the path of the field:

```scala mdoc
show(listener, "host" -> "0.0.0.0", "port" -> "80")
```

Combine it with `Config#mapKey` if the keys need a different case, as in `deriveConfig[Listener].mapKey(_.toUpperCase)`.

## Next Steps

[Errors](./errors.md) explains how a failed predicate is reported and pretty-printed. [Derivation](./automatic-derivation-of-config.md) explains how fields are derived.
