---
id: config-descriptors
title: "Config Descriptors"
sidebar_label: "Config Descriptors"
---

A config description is a value of type `Config[A]`. It says which keys to read and what type each key has. You build it from small pieces and combine them. Nothing is read until a `ConfigProvider` loads the description. This page lists the pieces.

Use this page when you want to write a description by hand. If your settings map directly to a case class, [derivation](./automatic-derivation-of-config.md) writes the description for you.

## Primitive descriptions

Each primitive reads one key:

```scala mdoc:silent
import zio._

val host: Config[String] = Config.string("HOST")
val port: Config[Int]    = Config.int("PORT")
val debug: Config[Boolean] = Config.boolean("DEBUG")
```

The key is the string you pass in. `Config.int("PORT")` reads the key `PORT` and fails with a format error when the value is not an integer.

## Combining descriptions

`zip` reads two descriptions and pairs their results. `to` turns a chain of `zip`s into a case class, and the field order must match:

```scala mdoc:silent
final case class Server(host: String, port: Int)

val server: Config[Server] =
  (Config.string("HOST") zip Config.int("PORT")).to[Server]
```

`map` changes the result of one description. `mapOrFail` does the same, but the conversion can fail with an error message:

```scala mdoc:silent
final case class Email(value: String)

val email: Config[Email] = Config.string("EMAIL").map(Email(_))

val positivePort: Config[Int] =
  Config.int("PORT").mapOrFail { port =>
    if (port > 0) Right(port)
    else Left(Config.Error.InvalidData(message = s"port must be positive, got $port"))
  }
```

## Optional values and defaults

`optional` makes a key optional. The result is an `Option`, and a missing key gives `None`:

```scala mdoc:silent
val region: Config[Option[String]] = Config.string("REGION").optional
```

`withDefault` uses a value when the key is missing:

```scala mdoc:silent
val timeoutSeconds: Config[Int] = Config.int("TIMEOUT").withDefault(30)
```

`orElse` tries a second description when the first one fails. `orElseEither` does the same and returns an `Either`, so you can tell which branch matched:

```scala mdoc:silent
val dbUrl: Config[String] =
  Config.string("DATABASE_URL") orElse Config.string("DB_URL")

val dbUrlOrPort: Config[Either[String, Int]] =
  Config.string("DB_URL").orElseEither(Config.int("DB_PORT"))
```

## Nested keys

`nested` puts a description under a prefix. Keys are joined with a dot, so `Config.string("url").nested("db")` reads `db.url`:

```scala mdoc:silent
final case class Database(url: String, poolSize: Int)

val database: Config[Database] =
  (Config.string("url") zip Config.int("poolSize")).to[Database].nested("db")
```

## Lists, tables and maps

`Config.listOf(name, item)` reads a list. Each element is read as an indexed key such as `servers[0]`, `servers[1]`, and so on:

```scala mdoc:silent
val servers: Config[List[String]] = Config.listOf("servers", Config.string)
```

`Config.table(name, item)` reads a map of keys under a prefix. Every key under `name` becomes a map entry, and its value is read with `item`:

```scala mdoc:silent
val labels: Config[Map[String, String]] = Config.table("labels", Config.string)
```

`Config.collectAll` reads a list of separate descriptions. Use it when each element has its own key prefix:

```scala mdoc:silent
val regions: Config[List[String]] =
  Config.collectAll(Config.string("REGION_1"), Config.string("REGION_2"), Config.string("REGION_3"))
```

## Constants

`Config.constant` always succeeds with the given value and reads nothing. It is useful inside `orElseEither` or when a sealed trait has a fixed value:

```scala mdoc:silent
val greeting: Config[String] = Config.constant("hello")
```

## Changing key names

`mapKey` changes every key in a description. `toKebabCase` converts the keys from camelCase to kebab-case. There are also `toSnakeCase`, `toUpperCase` and `toLowerCase`:

```scala mdoc:silent
val serverKebab: Config[Server] =
  (Config.string("hostName") zip Config.int("portNumber")).to[Server].toKebabCase
```

`mapKey` takes any function from key to key, for example `_.toUpperCase`.

## Reading from a provider

Give a description to a provider. The result is a `ZIO`:

```scala mdoc:compile-only
val source: ConfigProvider =
  ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "8080"))

val loaded: IO[Config.Error, Server] = source.load(server)
```

## Next steps

- [Config providers](./read-from-various-sources.md) lists the sources you can load from.
- [Derivation](./automatic-derivation-of-config.md) builds the same descriptions automatically.
- [Errors](./errors.md) explains what a failed load returns.
