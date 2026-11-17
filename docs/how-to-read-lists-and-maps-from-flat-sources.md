---
id: how-to-read-lists-and-maps-from-flat-sources
title: "How to Read Lists and Maps From Environment Variables"
sidebar_label: "Read Lists and Maps From Flat Sources"
---

Environment variables, system properties and plain maps are flat: every value is a string under a single name. This guide shows how to read lists, lists of objects and maps from such a flat source.

## Goal

Read this configuration from environment variables only:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Upstream(host: String, port: Int)

final case class Gateway(
  allowedOrigins: List[String],
  upstreams: List[Upstream],
  limits: Map[String, Int]
)

val gateway: Config[Gateway] = deriveConfig[Gateway].mapKey(key => toSnakeCase(key).toUpperCase)

def load[A](config: Config[A], provider: ConfigProvider): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(provider.load(config).either).getOrThrow()
  }
```

`Config#mapKey` turns the field names into upper snake case: `allowedOrigins` is read from `ALLOWED_ORIGINS`. Each step below uses `ConfigProvider.fromMap` with the same delimiters as `ConfigProvider.envProvider` (`_` between path segments), so that the example runs anywhere. With the real environment, use `ConfigProvider.envProvider` with the same variables.

## Step 1: Read a List of Strings From One Variable

ZIO's flat providers split a value on commas when it is read as a list. One variable holds the whole list:

```scala mdoc:silent
val origins = ConfigProvider.fromMap(Map("ALLOWED_ORIGINS" -> "https://a.example,https://b.example"), pathDelim = "_")
```

Reading it as a list splits the value:

```scala mdoc
load(Config.listOf("ALLOWED_ORIGINS", Config.string), origins)
```

To use another separator, pass `seqDelim`, as in `ConfigProvider.fromEnv(seqDelim = ";")`.

## Step 2: Read a List of Objects With Indexed Names

A list of objects needs one variable per field per element. Put the index in square brackets after the list's name, then the field: `UPSTREAMS[0]_HOST`, `UPSTREAMS[0]_PORT`, and so on:

```scala mdoc:silent
val upstreams = ConfigProvider.fromMap(
  Map(
    "UPSTREAMS[0]_HOST" -> "a.internal",
    "UPSTREAMS[0]_PORT" -> "8080",
    "UPSTREAMS[1]_HOST" -> "b.internal",
    "UPSTREAMS[1]_PORT" -> "8081"
  ),
  pathDelim = "_"
)
```

Each index becomes one element of the list:

```scala mdoc
load(Config.listOf("UPSTREAMS", deriveConfig[Upstream].mapKey(_.toUpperCase)), upstreams)
```

Most shells can't export a variable with `[` in its name, but container platforms and process managers, such as Docker and Kubernetes, can set it.

## Step 3: Read a Map

`Config.table`, or a `Map[String, A]` field, reads every variable under a prefix. The part of the name after the prefix becomes the key of the map:

```scala mdoc:silent
val limits = ConfigProvider.fromMap(Map("LIMITS_free" -> "10", "LIMITS_pro" -> "1000"), pathDelim = "_")
```

Every variable under `LIMITS` becomes a map entry:

```scala mdoc
load(Config.table("LIMITS", Config.int), limits)
```

The map keys keep their case. Because `_` separates path segments, a map key can't contain `_`.

## Step 4: Put It Together

All the variables together load the whole `Gateway`:

```scala mdoc:silent
val environment = ConfigProvider.fromMap(
  Map(
    "ALLOWED_ORIGINS"   -> "https://a.example,https://b.example",
    "UPSTREAMS[0]_HOST" -> "a.internal",
    "UPSTREAMS[0]_PORT" -> "8080",
    "LIMITS_free"       -> "10",
    "LIMITS_pro"        -> "1000"
  ),
  pathDelim = "_"
)
```

Loading the whole description:

```scala mdoc
load(gateway, environment)
```

## Notes

- A comma inside a single value always splits it with the default `seqDelim`. If a value can contain commas, choose another delimiter.
- Structured formats such as HOCON, YAML and TOML have real lists and don't need these conventions. Their providers don't split on commas unless you pass `enableCommaSeparatedValueAsList = true`. See [Config providers](./read-from-various-sources.md#lists-and-comma-separated-values).
- [ZIO core providers](./zio-core-providers.md) describes the environment provider and its delimiters.
