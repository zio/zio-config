---
id: toml-provider
title: "TOML"
sidebar_label: "TOML"
---

The `zio-config-toml` module reads [TOML](https://toml.io) with the [tomlj](https://github.com/tomlj/tomlj) parser and turns it into a `ConfigProvider`. It was added in ZIO Config 4.0.8.

To add the module:

```scala
libraryDependencies += "dev.zio" %% "zio-config-toml" % "@VERSION@"
```

## Building a Provider

`import zio.config.toml._` adds these constructors to `ConfigProvider`. The same functions, plus `fromToml` and `fromTomlZIO`, are on the `TomlConfigProvider` object:

| Constructor                                       | Reads                                              |
| ------------------------------------------------- | -------------------------------------------------- |
| `ConfigProvider.fromTomlString(tomlString)`       | A TOML string                                      |
| `ConfigProvider.fromTomlFile(file)`               | A `java.io.File`                                   |
| `ConfigProvider.fromTomlPath(path)`               | A `java.nio.file.Path`                             |
| `ConfigProvider.fromTomlReader(reader)`           | A `java.io.Reader`                                 |
| `TomlConfigProvider.fromToml(result)`             | A `TomlParseResult` that you parsed with tomlj     |

Each constructor parses its input when it is called. If the file is missing, it throws the `java.io.IOException` from tomlj. If the TOML has a syntax error, it throws a `RuntimeException` whose message starts with `Unable to parse TOML:` and lists tomlj's errors. Each one has a `...ZIO` variant, such as `ConfigProvider.fromTomlFileZIO` or `TomlConfigProvider.fromTomlZIO`, that returns a `Task[ConfigProvider]` and fails the task instead. Every constructor also takes `enableCommaSeparatedValueAsList` (default `false`).

The examples on this page share these definitions and a helper, `fromToml`, which loads a description from a TOML string:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.toml._

def fromToml[A](config: Config[A], toml: String, commaSeparated: Boolean = false): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromTomlString(toml, commaSeparated).load(config).either).getOrThrow()
  }

final case class Server(host: String, ports: List[Int])
final case class AppConfig(title: String, server: Server)
```

## Tables and Arrays

A TOML table becomes a nested key, and an array becomes a list. The keys under `[server]` are read with `Config#nested("server")`, which derivation adds for a field called `server`:

```scala mdoc
fromToml(deriveConfig[AppConfig], """
title = "orders"

[server]
host = "localhost"
ports = [8080, 8443]
""")
```

An inline table reads the same as a `[server]` table:

```scala mdoc
fromToml(deriveConfig[AppConfig], """
title = "orders"
server = { host = "localhost", ports = [8080] }
""")
```

An array of tables, written with `[[name]]`, is a list of case classes:

```scala mdoc
final case class Backend(url: String, weight: Int)

fromToml(Config.listOf("backends", deriveConfig[Backend]), """
[[backends]]
url = "http://a.internal"
weight = 3

[[backends]]
url = "http://b.internal"
weight = 1
""")
```

## Dates and Times

TOML has date and time values. The provider passes them on as their ISO-8601 text, so they load with `Config.localDate`, `Config.localDateTime`, `Config.localTime` and `Config.offsetDateTime`, or as plain strings:

```scala mdoc
final case class Release(day: java.time.LocalDate, at: java.time.OffsetDateTime)

fromToml(deriveConfig[Release], """
day = 2026-11-17
at = 2026-11-17T10:30:00Z
""")
```

## Comma-Separated Values

By default, a comma inside a string value is not split. Pass `enableCommaSeparatedValueAsList = true` to split such values when they are read as lists:

```scala mdoc
fromToml(Config.listOf("ports", Config.int), """ports = "8080,8443"""", commaSeparated = true)
```

A TOML array is a list with or without the flag.

## Using a tomlj Result

If you parse the TOML yourself, for example to validate it first, pass the `TomlParseResult` to `TomlConfigProvider.fromToml`:

```scala mdoc:compile-only
import org.tomlj.Toml
import zio.config.toml.TomlConfigProvider

val result   = Toml.parse("host = \"localhost\"")
val provider = TomlConfigProvider.fromToml(result)
```

## Next Steps

- [Config providers](./read-from-various-sources.md) compares every source.
- [Config descriptors](./config-descriptors.md) describes the keys a provider reads.
