---
id: hocon-provider
title: "HOCON and JSON"
sidebar_label: "HOCON and JSON"
---

The `zio-config-typesafe` module reads HOCON, the format of the [Typesafe Config](https://github.com/lightbend/config) library, and turns it into a `ConfigProvider`. HOCON is a superset of JSON, so the same provider reads JSON documents.

To add the module:

```scala
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
```

## Building a Provider

`import zio.config.typesafe._` adds these constructors to `ConfigProvider`. The same functions are on the `TypesafeConfigProvider` object:

| Constructor                                      | Reads                                                                        |
| ------------------------------------------------ | ---------------------------------------------------------------------------- |
| `ConfigProvider.fromHoconString(input)`          | A HOCON or JSON string                                                       |
| `ConfigProvider.fromHoconFile(file)`             | A `java.io.File`                                                             |
| `ConfigProvider.fromHoconFilePath(path)`         | A file, given its path as a `String`                                         |
| `ConfigProvider.fromResourcePath()`              | `application.conf` and `reference.conf` on the classpath, with system-property overrides (`ConfigFactory.load`) |
| `ConfigProvider.fromTypesafeConfig(config)`      | A `com.typesafe.config.Config` you have already built                        |

Each constructor reads and parses its input when it is called, and throws if the input is missing or malformed. Each one has a `...ZIO` variant, such as `ConfigProvider.fromHoconFileZIO`, that returns a `Task[ConfigProvider]` and fails the task instead. Every constructor also takes `enableCommaSeparatedValueAsList`, described below.

The examples on this page share these definitions and a helper, `fromHocon`, which loads a description from a HOCON string:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

def fromHocon[A](config: Config[A], hocon: String, commaSeparated: Boolean = false): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromHoconString(hocon, commaSeparated).load(config).either).getOrThrow()
  }

final case class Database(url: String, poolSize: Int)
final case class AppConfig(name: String, database: Database, replicas: List[String])
```

## HOCON Objects and Arrays

A HOCON object becomes a nested key, and an array becomes a list. HOCON accepts `=` or `:` between a key and its value, dotted keys such as `database.url`, and newlines or commas between entries:

```scala mdoc
fromHocon(deriveConfig[AppConfig], """
  name = "orders"
  database {
    url = "postgres://localhost/orders"
    poolSize = 10
  }
  replicas = ["eu-1", "eu-2"]
""")
```

The same description reads JSON:

```scala mdoc
fromHocon(deriveConfig[AppConfig], """
  {
    "name": "orders",
    "database": { "url": "postgres://localhost/orders", "poolSize": 10 },
    "replicas": ["eu-1", "eu-2"]
  }
""")
```

An array of objects is a list of case classes, and an empty array is an empty list:

```scala mdoc
fromHocon(Config.listOf("databases", deriveConfig[Database]), """
  databases = [
    { url = "postgres://a/app", poolSize = 5 },
    { url = "postgres://b/app", poolSize = 7 }
  ]
""")

fromHocon(Config.listOf("databases", deriveConfig[Database]), "databases = []")
```

## Substitutions and Environment Variables

The provider resolves HOCON substitutions before reading. `${path}` refers to another key, and `${?NAME}` refers to an environment variable or system property when it is set. A common pattern keeps a default in the file and lets the environment override it:

```scala mdoc
final case class Http(host: String, port: Int)

fromHocon(deriveConfig[Http], """
  defaults { host = "0.0.0.0" }
  host = ${defaults.host}
  port = 8080
  port = ${?ZIO_CONFIG_DOCS_UNSET_PORT}
""")
```

Here `ZIO_CONFIG_DOCS_UNSET_PORT` is not set, so `port` keeps `8080`. In a deployment, setting the variable replaces the value.

## Null Values

An explicit `null` in HOCON is read as an empty string, not as a missing key:

```scala mdoc
fromHocon(Config.string("region").optional, "region = null")
```

To make a setting absent, leave the key out instead of setting it to `null`.

## Comma-Separated Values

By default, a string value is never split. A value such as `"a,b"` read with `Config.listOf` is one element, or fails when the element type is a number. Pass `enableCommaSeparatedValueAsList = true` to split such values on commas:

```scala mdoc:silent
val ports: Config[List[Int]] = Config.listOf("ports", Config.int)
```

Without the flag the value fails to parse as a list of integers; with it, the value is split:

```scala mdoc
fromHocon(ports, """ports = "8080,8443"""")
fromHocon(ports, """ports = "8080,8443"""", commaSeparated = true)
```

HOCON arrays are lists with or without the flag.

## Loading From the Classpath

`ConfigProvider.fromResourcePath()` uses `ConfigFactory.load` from Typesafe Config. It reads `application.conf` (and `application.json` or `application.properties`) from the classpath, falls back to `reference.conf`, and lets JVM system properties override any key, so `-Ddatabase.poolSize=20` on the command line wins over the file. Its standard Typesafe Config switches also work, such as `-Dconfig.file=/etc/app.conf` to read another file:

```scala mdoc:compile-only
val fromClasspath: ConfigProvider = ConfigProvider.fromResourcePath()

val fromClasspathSafely: Task[ConfigProvider] = ConfigProvider.fromResourcePathZIO()
```

## Using an Existing Typesafe Config

If your application already builds a `com.typesafe.config.Config`, for example to merge several files, pass it to `ConfigProvider.fromTypesafeConfig`:

```scala mdoc:compile-only
import com.typesafe.config.ConfigFactory

val merged: com.typesafe.config.Config =
  ConfigFactory.parseString("port = 9090").withFallback(ConfigFactory.parseString("host = localhost, port = 8080")).resolve()

val provider: ConfigProvider = ConfigProvider.fromTypesafeConfig(merged)
```

## Next Steps

- [Config providers](./read-from-various-sources.md) compares every source.
- [Tutorial: configure an app from HOCON and environment variables](./tutorial-hocon-and-env.md) builds a full setup.
- [ZIO core providers](./zio-core-providers.md) shows how to layer this provider with others.
