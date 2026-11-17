---
id: zio-core-providers
title: "ZIO Core Providers"
sidebar_label: "ZIO Core Providers"
---

ZIO itself ships the `ConfigProvider` type and several providers: environment variables, system properties, command-line arguments and maps. It also has a built-in way to read configuration anywhere in a program, `ZIO.config`. ZIO Config builds on both: every provider from the ZIO Config modules is an ordinary `ConfigProvider`, so everything on this page works with HOCON, YAML, TOML, XML and AWS sources too.

## Reading Config With ZIO.config

`ZIO.config(description)` loads a description from the provider that the ZIO runtime holds. By default that provider is `ConfigProvider.defaultProvider`, which reads environment variables first and system properties second. To use another source for the whole program, set it once with `Runtime.setConfigProvider` in the `bootstrap` layer of your `ZIOAppDefault`:

```scala mdoc:compile-only
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class AppConfig(host: String, port: Int)

object Main extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(ConfigProvider.fromResourcePath())

  def run =
    for {
      config <- ZIO.config(deriveConfig[AppConfig])
      _      <- Console.printLine(s"Starting on ${config.host}:${config.port}")
    } yield ()
}
```

`ZIO.config` returns a `ZIO[Any, Config.Error, A]`. When the description is available as an implicit `Config[A]`, the short form `ZIO.config[AppConfig]` finds it.

A common pattern turns a description into a layer, so that services receive their settings through the environment:

```scala mdoc:compile-only
import zio._
import zio.config.magnolia.deriveConfig

final case class DatabaseConfig(url: String, poolSize: Int)

object DatabaseConfig {
  val layer: ZLayer[Any, Config.Error, DatabaseConfig] =
    ZLayer.fromZIO(ZIO.config(deriveConfig[DatabaseConfig].nested("database")))
}
```

To run one effect with another provider, for example in a test, use `ZIO.withConfigProvider`:

```scala mdoc:compile-only
import zio._

val port: IO[Config.Error, Int] =
  ZIO.withConfigProvider(ConfigProvider.fromMap(Map("port" -> "9999")))(ZIO.config(Config.int("port")))
```

`ConfigProvider#load` and `ZIO.config` read the same description the same way. Use `ConfigProvider#load` when you hold the provider yourself, and `ZIO.config` when the provider is set for the whole program.

## Built-in Providers

The examples below share these definitions and a helper, `show`, which loads a description from a provider:

```scala mdoc:silent
import zio._
import zio.config._

final case class Database(url: String, poolSize: Int)

val database: Config[Database] =
  (Config.string("url") zip Config.int("poolSize")).to[Database].nested("db")

def show[A](provider: ConfigProvider, config: Config[A]): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(provider.load(config).either).getOrThrow()
  }
```

ZIO core has these providers. The path delimiter joins the segments of a nested key, and the sequence delimiter splits one value into a list:

| Provider                                         | Reads                         | Path delimiter | Sequence delimiter |
| ------------------------------------------------ | ----------------------------- | -------------- | ------------------ |
| `ConfigProvider.fromMap(map, pathDelim, seqDelim)` | A `Map[String, String]`       | `.`            | `,`                |
| `ConfigProvider.fromEnv(pathDelim, seqDelim)`    | Environment variables         | `_`            | `,`                |
| `ConfigProvider.envProvider`                     | Same as `fromEnv()`           | `_`            | `,`                |
| `ConfigProvider.fromProps(pathDelim, seqDelim)`  | JVM system properties         | `.`            | `,`                |
| `ConfigProvider.propsProvider`                   | Same as `fromProps()`         | `.`            | `,`                |
| `ConfigProvider.fromAppArgs(args, pathDelim, seqDelim)` | Command-line arguments  | `.`            | none               |
| `ConfigProvider.defaultProvider`                 | `envProvider`, then `propsProvider` | —        | —                  |

Every one of them splits values on commas by default. The providers from the ZIO Config modules don't; see [Config providers](./read-from-various-sources.md).

### Maps

`ConfigProvider.fromMap` reads a `Map[String, String]`. It is the simplest provider and the usual choice in tests:

```scala mdoc
show(ConfigProvider.fromMap(Map("db.url" -> "postgres://localhost/app", "db.poolSize" -> "10")), database)
```

A value that holds commas is split when it is read as a list:

```scala mdoc
show(ConfigProvider.fromMap(Map("ports" -> "8080,8443")), Config.listOf("ports", Config.int))
```

### Environment Variables

`ConfigProvider.envProvider` joins the segments of a path with `_` and looks the result up as is, so the key names are case-sensitive. The description above reads `db_url` and `db_poolSize`. Environment variables are usually written in upper case, so combine the provider with `ConfigProvider#upperCase` and, for camelCase field names, `ConfigProvider#snakeCase`. The environment can't be changed inside these docs, so the next example uses `ConfigProvider.fromMap` with the same delimiter:

```scala mdoc:silent
val envLike: ConfigProvider =
  ConfigProvider.fromMap(Map("DB_URL" -> "postgres://db/app", "DB_POOL_SIZE" -> "20"), pathDelim = "_")
```

With `ConfigProvider#snakeCase` and `ConfigProvider#upperCase`, the key `db.poolSize` is looked up as `DB_POOL_SIZE`:

```scala mdoc
show(envLike.snakeCase.upperCase, database)
```

With the real environment, the same provider is `ConfigProvider.envProvider.snakeCase.upperCase`.

### Command-Line Arguments

`ConfigProvider.fromAppArgs` reads arguments of the form `--key=value` or `--key value`. In a `ZIOAppDefault`, get the arguments with `ZIOAppDefault#getArgs`:

```scala mdoc
show(ConfigProvider.fromAppArgs(ZIOAppArgs(Chunk("--db.url=postgres://cli/app", "--db.poolSize", "5"))), database)
```

## Combining and Adapting Providers

Providers compose. These functions on `ConfigProvider` return a new provider:

| Function                             | Effect                                                                                       |
| ------------------------------------ | -------------------------------------------------------------------------------------------- |
| `ConfigProvider#orElse(that)`        | Use `that` for every key this provider can't supply                                          |
| `ConfigProvider#nested(name)`        | Read everything from under the prefix `name` of this source                                  |
| `ConfigProvider#unnested(name)`      | Strip the prefix `name` from the keys the description asks for                               |
| `ConfigProvider#upperCase`           | Upper-case the keys before looking them up                                                   |
| `ConfigProvider#lowerCase`           | Lower-case the keys before looking them up                                                   |
| `ConfigProvider#snakeCase`           | Convert camelCase keys to snake_case before looking them up                                  |
| `ConfigProvider#kebabCase`           | Convert camelCase keys to kebab-case before looking them up                                  |
| `ConfigProvider#contramapPath(f)`    | Apply any `String => String` to each key segment before looking it up                        |
| `ConfigProvider#within(path)(f)`     | Apply the transformation `f` only to the part of the source under `path`                     |

`ConfigProvider#orElse` is how you layer sources. The first provider wins for every key it has:

```scala mdoc:silent
val overrides = ConfigProvider.fromMap(Map("db.poolSize" -> "50"))
val defaults  = ConfigProvider.fromMap(Map("db.url" -> "postgres://localhost/app", "db.poolSize" -> "10"))
```

`poolSize` comes from `overrides` and `url` from `defaults`:

```scala mdoc
show(overrides.orElse(defaults), database)
```

`ConfigProvider#nested` reads a whole description from one part of a source, for example the `staging` part of a shared map:

```scala mdoc:silent
val shared = ConfigProvider.fromMap(Map("staging.db.url" -> "postgres://staging/app", "staging.db.poolSize" -> "3"))
```

Nested under `staging`, the provider answers the description's `db.url` with `staging.db.url`:

```scala mdoc
show(shared.nested("staging"), database)
```

The case conversions on a provider come from ZIO, and since ZIO 2.1.20 they split digits as their own word. ZIO Config's `Config#toKebabCase` and the `@kebabCase` annotation don't, so the two can disagree for names with digits:

```scala mdoc:silent
val digits = Config.string("myValue123")
```

The provider looks the key up as `my-value-123`, while the description's own conversion asks for `my-value123`:

```scala mdoc
show(ConfigProvider.fromMap(Map("my-value-123" -> "from provider")).kebabCase, digits)
show(ConfigProvider.fromMap(Map("my-value123" -> "from description")), digits.toKebabCase)
```

Pick one place to convert key names, either the description or the provider, and use it throughout.

## Next Steps

- [Config providers](./read-from-various-sources.md) compares every source, including the ZIO Config modules.
- [How to override settings per environment](./how-to-override-per-environment.md) layers providers for each deployment.
- [How to test config](./how-to-test-config.md) uses `ConfigProvider.fromMap` and `ZIO.withConfigProvider` in tests.
