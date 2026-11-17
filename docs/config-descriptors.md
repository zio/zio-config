---
id: config-descriptors
title: "Config Descriptors"
sidebar_label: "Config Descriptors"
---

A config description is a value of type `Config[A]`. It says which keys to read, what type each key has and how the values combine into an `A`. Nothing is read until a `ConfigProvider` loads the description. This page lists the building blocks.

Use this page when you write a description by hand. If your settings map directly to a case class, [derivation](./automatic-derivation-of-config.md) writes the description for you, and every function on this page still works on the result.

## Where the Building Blocks Come From

`Config` and `ConfigProvider` are part of ZIO itself. ZIO Config adds more building blocks on top of them. They become available after `import zio.config._`:

| Comes from                     | Examples                                                                                                                |
| ------------------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| ZIO core (`zio.Config`)        | `Config.string`, `Config.int`, `Config#zip`, `Config#nested`, `Config#optional`, `Config#withDefault`, `Config.listOf`  |
| ZIO Config (`zio.config._`)    | `Config.byte`, `Config.short`, `Config.uuid`, `Config.constant`, `Config.collectAll`, `Config#to`, `Config#mapKey`, `Config#orElseEither`, `Config#toKebabCase` |

If the compiler reports that a method such as `Config#to` or `Config#mapKey` "is not a member of zio.Config", add `import zio.config._`.

The examples on this page share these imports and a small helper, `show`, which loads a description from a map and returns either the error or the value:

```scala mdoc:silent
import zio._
import zio.config._

def show[A](config: Config[A], entries: (String, String)*): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromMap(entries.toMap).load(config).either).getOrThrow()
  }
```

## Primitive Descriptions

Each primitive reads one value. With a name, it reads the key of that name. Without a name, it reads the value at the current path, which is useful inside `Config#nested`, `Config.listOf` or `Config.table`:

```scala mdoc:silent
val host: Config[String]   = Config.string("HOST")
val port: Config[Int]      = Config.int("PORT")
val debug: Config[Boolean] = Config.boolean("DEBUG")
```

`Config.int("PORT")` reads the key `PORT` and fails with an `InvalidData` error when the value is not an integer:

```scala mdoc
show(port, "PORT" -> "8080")
show(port, "PORT" -> "eighty")
```

ZIO core provides these primitives. Each one has a form with a name and a form without:

| Primitive                 | Result type                   |
| ------------------------- | ----------------------------- |
| `Config.string`           | `String`                      |
| `Config.boolean`          | `Boolean`                     |
| `Config.int`              | `Int`                         |
| `Config.long`             | `Long`                        |
| `Config.bigInt`           | `BigInt`                      |
| `Config.float`            | `Float`                       |
| `Config.double`           | `Double`                      |
| `Config.bigDecimal`       | `BigDecimal`                  |
| `Config.duration`         | `zio.Duration`                |
| `Config.uri`              | `java.net.URI`                |
| `Config.localDate`        | `java.time.LocalDate`         |
| `Config.localTime`        | `java.time.LocalTime`         |
| `Config.localDateTime`    | `java.time.LocalDateTime`     |
| `Config.offsetDateTime`   | `java.time.OffsetDateTime`    |
| `Config.logLevel`         | `zio.LogLevel`                |
| `Config.secret`           | `Config.Secret` (hidden from `toString`) |

ZIO Config adds `Config.byte`, `Config.short` and `Config.uuid`. They have no form that takes a name, so put them under a key with `Config#nested`:

```scala mdoc:silent
val requestId: Config[java.util.UUID] = Config.uuid.nested("REQUEST_ID")
```

Loading it from a map with a valid UUID:

```scala mdoc
show(requestId, "REQUEST_ID" -> "1b4e28ba-2fa1-11d2-883f-0016d3cca427")
```

## Combining Descriptions

`Config#zip` reads two descriptions and pairs their results. Chained calls to `Config#zip` build a flat tuple, and `Config#to` turns that tuple into a case class whose fields are in the same order:

```scala mdoc:silent
final case class Server(host: String, port: Int)

val server: Config[Server] =
  (Config.string("HOST") zip Config.int("PORT")).to[Server]
```

Loading it from a map with both keys:

```scala mdoc
show(server, "HOST" -> "localhost", "PORT" -> "8080")
```

`Config#to` comes from ZIO Config. With ZIO core alone, call `Config#map` with a function that builds the case class from the tuple.

## Transforming and Validating Values

To change the type of a value or to reject some values, use one of these functions:

| Function                     | Use it when                                                                    |
| ---------------------------- | ------------------------------------------------------------------------------ |
| `Config#map`                 | The conversion always succeeds                                                 |
| `Config#mapAttempt`          | The conversion can throw; the exception becomes an `InvalidData` error         |
| `Config#mapOrFail`           | The conversion returns `Either[Config.Error, B]`                               |
| `Config#validate`            | You keep the value but check a predicate, with an error message for failures   |
| `Config#validateWith`        | You check and convert at once with a partial function                          |

To keep a port only when it is positive, and to turn a string into a wrapper type:

```scala mdoc:silent
final case class Email(value: String)

val email: Config[Email] = Config.string("EMAIL").map(Email(_))

val positivePort: Config[Int] =
  Config.int("PORT").validate("port must be positive")(_ > 0)
```

A negative port fails the validation with its message:

```scala mdoc
show(positivePort, "PORT" -> "-1")
```

`Config#mapOrFail` gives full control over the error:

```scala mdoc:silent
val evenPort: Config[Int] =
  Config.int("PORT").mapOrFail { port =>
    if (port % 2 == 0) Right(port)
    else Left(Config.Error.InvalidData(message = s"port must be even, got $port"))
  }
```

## Optional Values, Defaults and Alternatives

`Config#optional` makes a key optional. The result is an `Option`, and a missing key gives `None`. A key that is present but invalid still fails:

When `REGION` is missing from the source, `Config.string("REGION").optional` loads `None`:

```scala mdoc
show(Config.string("REGION").optional)
```

`Config#withDefault` uses a value when the key is missing:

```scala mdoc:silent
val timeoutSeconds: Config[Int] = Config.int("TIMEOUT").withDefault(30)
```

When `TIMEOUT` is missing, the default is used:

```scala mdoc
show(timeoutSeconds)
```

`Config#orElse` tries a second description when the first one fails. `Config#orElseEither`, from ZIO Config, does the same and returns an `Either`, so you can tell which branch matched:

```scala mdoc:silent
val dbUrl: Config[String] =
  Config.string("DATABASE_URL") orElse Config.string("DB_URL")

val dbUrlOrPort: Config[Either[String, Int]] =
  Config.string("DB_URL").orElseEither(Config.int("DB_PORT"))
```

When only `DB_PORT` is set, the result is the right side:

```scala mdoc
show(dbUrlOrPort, "DB_PORT" -> "5432")
```

## Nested Keys

`Config#nested` puts a description under a prefix. `Config.string("url").nested("db")` reads the key `db.url` from a map. Each provider decides how the path is written in its source: HOCON, YAML and TOML use nested objects, a map uses a dot, and environment variables use `_`:

```scala mdoc:silent
final case class Database(url: String, poolSize: Int)

val database: Config[Database] =
  (Config.string("url") zip Config.int("poolSize")).to[Database].nested("db")
```

Loading it from a map with dotted keys:

```scala mdoc
show(database, "db.url" -> "postgres://localhost/app", "db.poolSize" -> "10")
```

`Config#nested` also takes several names. `config.nested("app", "db")` is the same as `config.nested("db").nested("app")`, and it reads `app.db.url`.

## Lists, Sets, Maps and Tables

ZIO core reads collections from indexed keys such as `servers[0]`, `servers[1]`, from a list in a structured source such as HOCON, or from a comma-separated value when the provider splits values:

| Function                  | Result type             |
| ------------------------- | ----------------------- |
| `Config.listOf`           | `List[A]`               |
| `Config.setOf`            | `Set[A]`                |
| `Config.vectorOf`         | `Vector[A]`             |
| `Config.chunkOf`          | `Chunk[A]`              |
| `Config.nonEmptyChunkOf`  | `NonEmptyChunk[A]`      |
| `Config.table`            | `Map[String, A]`        |

Each function takes the name of the key and a description of one element:

```scala mdoc:silent
val servers: Config[List[String]] = Config.listOf("servers", Config.string)
```

Loading it from indexed keys:

```scala mdoc
show(servers, "servers[0]" -> "a.example.com", "servers[1]" -> "b.example.com")
```

The element can be a whole structure. Here each element of the list is a `Server`, read from `HOST` and `PORT` under its index:

```scala mdoc:silent
val serverList: Config[List[Server]] = Config.listOf("servers", server)
```

Each index holds one `Server`:

```scala mdoc
show(serverList, "servers[0].HOST" -> "a", "servers[0].PORT" -> "1", "servers[1].HOST" -> "b", "servers[1].PORT" -> "2")
```

`Config.table` reads every key under a prefix as a map entry:

```scala mdoc:silent
val labels: Config[Map[String, String]] = Config.table("labels", Config.string)
```

Every key under `labels` becomes an entry:

```scala mdoc
show(labels, "labels.team" -> "payments", "labels.tier" -> "1")
```

`Config.collectAll`, from ZIO Config, reads a fixed list of separate descriptions into a `List`. Use it when each element has its own key:

```scala mdoc:silent
val regions: Config[List[String]] =
  Config.collectAll(Config.string("REGION_1"), Config.string("REGION_2"))
```

Both keys end up in one list:

```scala mdoc
show(regions, "REGION_1" -> "eu-west-1", "REGION_2" -> "us-east-1")
```

## Choosing a Description by Value

`Config#switch` reads a value first and then picks the description to read next. It is the hand-written form of a sealed trait with a type field:

```scala mdoc:silent
sealed trait Store
final case class InMemory()             extends Store
final case class Postgres(url: String)  extends Store

val store: Config[Store] =
  Config.string("type").switch(
    "memory"   -> Config.succeed(InMemory()),
    "postgres" -> Config.string("url").map(Postgres(_))
  )
```

With `type = postgres`, the second description is read:

```scala mdoc
show(store, "type" -> "postgres", "url" -> "postgres://localhost/app")
```

## Fixed Values

Two functions produce a value without reading a key, and one checks a key against a fixed string:

| Function            | Comes from | Behaviour                                                                              |
| ------------------- | ---------- | -------------------------------------------------------------------------------------- |
| `Config.succeed`    | ZIO core   | Always succeeds with the given value and reads nothing                                 |
| `Config.fail`       | ZIO core   | Always fails with the given message                                                    |
| `Config.constant`   | ZIO Config | Reads a string at the current path and succeeds only when it equals the given string |

`Config.constant` is what derivation uses for a case object: `greeting = Hello` matches `Config.constant("Hello")`. It is not a way to supply a fixed value, because it fails when the key is missing:

```scala mdoc
show(Config.constant("Hello").nested("greeting"), "greeting" -> "Hello")
show(Config.constant("Hello").nested("greeting"))
show(Config.succeed("Hello"))
```

## Describing Keys

`Config#??` attaches a description to a key. The text appears in [generated documentation](./documentation-and-reporting.md) and does not change how the key is read:

```scala mdoc:silent
val documentedPort: Config[Int] = Config.int("PORT") ?? "Port the HTTP server listens on"
```

## Recursive Descriptions

`Config.defer` delays building a description until it is used. It lets a description refer to itself, for example for a tree of categories:

```scala mdoc:silent
final case class Category(name: String, children: List[Category])

lazy val category: Config[Category] =
  (Config.string("name") zip Config.listOf("children", Config.defer(category))).to[Category]
```

In a flat map, the value `<nil>` marks an empty list, so the leaf category ends the recursion:

```scala mdoc
show(category, "name" -> "root", "children[0].name" -> "leaf", "children[0].children" -> "<nil>")
```

Derivation does not handle recursive types, so recursive descriptions are written by hand. [Documentation generation](./documentation-and-reporting.md) does not support them yet.

## Changing Key Names

ZIO Config adds functions that change every key in a description, which is useful when the names in the source follow another convention than your code:

| Function                 | Effect on the key `poolSize`            |
| ------------------------ | --------------------------------------- |
| `Config#toKebabCase`     | `pool-size`                             |
| `Config#toSnakeCase`     | `pool_size`                             |
| `Config#toUpperCase`     | `POOLSIZE`                              |
| `Config#toLowerCase`     | `poolsize`                              |
| `Config#mapKey(f)`       | whatever `f` returns                    |

To read the `Database` description from snake_case keys:

```scala mdoc
show(database.toSnakeCase, "db.url" -> "postgres://localhost/app", "db.pool_size" -> "10")
```

`Config#mapKey` applies a function to every key, including the prefixes added by `Config#nested`. Combine the built-in conversions to get keys such as `DB_POOL_SIZE`:

```scala mdoc:silent
val upperSnake: Config[Database] = database.mapKey(key => toSnakeCase(key).toUpperCase)
```

The same conversions are available as plain `String => String` functions in `zio.config._`:

| Function                          | Example                                           |
| --------------------------------- | ------------------------------------------------- |
| `toKebabCase`                     | `poolSize` → `pool-size`, `version2Name` → `version2-name` |
| `toKebabCaseLegacy`               | `version2Name` → `version2name` (splits only at a lowercase-to-uppercase boundary) |
| `toSnakeCase`                     | `poolSize` → `pool_size`                          |
| `camelToDelimiter(s, delimiter)`  | `camelToDelimiter("poolSize", ".")` → `pool.size` |
| `addPrefixToKey(prefix)`          | `addPrefixToKey("db")("url")` → `dbUrl`           |
| `addPostFixToKey(postfix)`        | `addPostFixToKey("value")("url")` → `urlValue`    |

The conversions on this page change keys in the description. A `ConfigProvider` can change keys on the source side instead, with `ConfigProvider#kebabCase`, `ConfigProvider#snakeCase`, `ConfigProvider#upperCase` and `ConfigProvider#lowerCase`. See [Config providers](./read-from-various-sources.md).

## Reading a Description

Give a description to a provider. The result is a `ZIO` that fails with a `Config.Error`:

```scala mdoc:compile-only
val source: ConfigProvider =
  ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "8080"))

val loaded: IO[Config.Error, Server] = source.load(server)
```

In a ZIO application you usually set the provider once and read descriptions with `ZIO.config`. See [ZIO core providers](./zio-core-providers.md).

ZIO Config 4 also keeps the reading style of ZIO Config 3, `read(description from provider)`. It returns the same `IO[Config.Error, A]`, and `Config#from` also accepts a `Task[ConfigProvider]`, such as the result of `ConfigProvider.fromHoconFileZIO`:

```scala mdoc:compile-only
val sameAsLoad: IO[Config.Error, Server] =
  read(server from ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "8080")))
```

## Next Steps

- [Config providers](./read-from-various-sources.md) lists the sources you can load from.
- [Derivation](./automatic-derivation-of-config.md) builds the same descriptions automatically.
- [Errors](./errors.md) explains what a failed load returns.
