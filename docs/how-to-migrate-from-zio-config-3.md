---
id: how-to-migrate-from-zio-config-3
title: "How to Migrate from ZIO Config 3.x"
sidebar_label: "Migrate from ZIO Config 3.x"
---

ZIO Config 4 builds on the `Config` and `ConfigProvider` types that ZIO 2 added to its core. ZIO Config 3 had its own types for the same jobs: `ConfigDescriptor` and `ConfigSource`. This guide moves a ZIO Config 3 project to version 4, one concept at a time.

## Goal

Compile and run a ZIO Config 3 application on ZIO Config 4 with the same configuration files and keys.

## Step 1: Update the Dependencies

All modules move to the same 4.x version. The module names are unchanged:

```scala
// ZIO Config core: config descriptors and readers (JVM)
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"

// HOCON and JSON sources, built on Typesafe Config (JVM)
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"

// Automatic derivation of config descriptors (JVM)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

ZIO Config 4 needs ZIO 2.

## Step 2: Replace `ConfigDescriptor` With `Config`

A description is now a `zio.Config[A]`. The primitive constructors move from `ConfigDescriptor` to the `Config` companion, and `zip` with `to[A]` still builds a case class:

**Before (ZIO Config 3):**

```scala
import zio.config._, ConfigDescriptor._

final case class MyConfig(ldap: String, port: Int, dburl: String)

val myConfig: ConfigDescriptor[MyConfig] =
  (string("LDAP") zip int("PORT") zip string("DB_URL")).to[MyConfig]
```

The 4.x version uses `Config.string` and `Config.int`; `Config#to` comes from `import zio.config._`:

```scala mdoc:silent
import zio._
import zio.config._

final case class MyConfig(ldap: String, port: Int, dburl: String)

val myConfig: Config[MyConfig] =
  (Config.string("LDAP") zip Config.int("PORT") zip Config.string("DB_URL")).to[MyConfig]
```

Nesting changes shape. In 3.x, `nested("database")(string("url"))` took the description as a second argument; in 4.x, `Config#nested` is a method on the description:

```scala mdoc:silent
val databaseUrl: Config[String] = Config.string("url").nested("database")
```

The other common renames:

| ZIO Config 3                              | ZIO Config 4                                            |
| ----------------------------------------- | ------------------------------------------------------- |
| `ConfigDescriptor[A]`                     | `Config[A]`                                             |
| `string("K")`, `int("K")`, …              | `Config.string("K")`, `Config.int("K")`, …              |
| `nested("p")(desc)`                       | `desc.nested("p")`                                      |
| `list("K")(desc)`                         | `Config.listOf("K", desc)`                              |
| `map("K")(desc)`                          | `Config.table("K", desc)`                               |
| `desc.default(v)`                         | `desc.withDefault(v)`                                   |
| `desc.optional`                           | `desc.optional`                                         |
| `desc ?? "text"`                          | `desc ?? "text"`                                        |
| `desc.transformOrFail(...)`               | `desc.mapOrFail(...)` (no reverse function)             |
| `desc.mapKey(f)`, `toKebabCase`           | Unchanged, from `import zio.config._`                   |

Descriptions in 4.x only read. ZIO Config 3 could also write a value back to a source with `write`; ZIO Config 4 has no `write`, so the reverse functions of `transform` and `transformOrFail` go away.

## Step 3: Replace `descriptor[A]` With `deriveConfig[A]`

Derivation keeps its rules and annotations. Only the entry point changes:

**Before (ZIO Config 3):**

```scala
import zio.config.magnolia._

val derived = descriptor[MyConfig]
```

The 4.x entry point is `deriveConfig`:

```scala mdoc:silent
import zio.config.magnolia.deriveConfig

val derived: Config[MyConfig] = deriveConfig[MyConfig]
```

Custom `Descriptor[A]` instances become `DeriveConfig[A]` instances. See the [DeriveConfig reference](./derive-config.md).

## Step 4: Replace `ConfigSource` With `ConfigProvider`

A source is now a `zio.ConfigProvider`. The module-specific constructors keep their names on `ConfigProvider`:

| ZIO Config 3                                     | ZIO Config 4                                                   |
| ------------------------------------------------ | -------------------------------------------------------------- |
| `ConfigSource.fromMap(map)`                      | `ConfigProvider.fromMap(map)`                                  |
| `ConfigSource.fromSystemEnv()`                   | `ConfigProvider.envProvider` or `ConfigProvider.fromEnv(...)` |
| `ConfigSource.fromSystemProps()`                 | `ConfigProvider.propsProvider`                                 |
| `TypesafeConfigSource.fromHoconString(s)`        | `ConfigProvider.fromHoconString(s)` with `zio.config.typesafe._` |
| `TypesafeConfigSource.fromResourcePath`          | `ConfigProvider.fromResourcePath()`                            |
| `YamlConfigSource.fromYamlString(s)`             | `ConfigProvider.fromYamlString(s)` with `zio.config.yaml._`    |
| `source1 <> source2`, `source1.orElse(source2)`  | `provider1.orElse(provider2)`                                  |

## Step 5: Keep `read(... from ...)`, or Move to `load`

The 3.x way of reading still compiles in 4.x: `read(description from provider)` returns an `IO[Config.Error, A]`. It is the quickest way to get a 3.x code base compiling:

```scala mdoc:silent
val source: ConfigProvider =
  ConfigProvider.fromMap(Map("LDAP" -> "ldap://localhost", "PORT" -> "389", "DB_URL" -> "postgres://localhost/app"))

val viaRead: IO[Config.Error, MyConfig] = read(myConfig from source)
```

Both styles read the same values:

```scala mdoc
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(viaRead zip source.load(myConfig)).getOrThrow()
}
```

New code uses `ConfigProvider#load`, or `ZIO.config` with a provider set for the whole application.

## Step 6: Replace `ZConfig` Layers

ZIO Config 3 built layers with `ZConfig.fromSystemEnv(desc)` and similar functions, and read them with `getConfig[A]`. In 4.x, build a layer from `ZIO.config` and set the provider once:

**Before (ZIO Config 3):**

```scala
val layer: Layer[ReadError[String], MyConfig] = ZConfig.fromSystemEnv(myConfig)
```

The 4.x version reads with `ZIO.config` and gets its source from the runtime:

```scala mdoc:compile-only
val layer: ZLayer[Any, Config.Error, MyConfig] = ZLayer.fromZIO(ZIO.config(myConfig))

object Main extends ZIOAppDefault {
  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(ConfigProvider.envProvider)

  def run = ZIO.serviceWithZIO[MyConfig](config => Console.printLine(config.ldap)).provide(layer)
}
```

See [ZIO core providers](./zio-core-providers.md) for the details.

## Step 7: Replace `ReadError` With `Config.Error`

Errors are now `zio.Config.Error`. `ReadError.MissingValue` becomes `MissingData`. `FormatError`, and the `ConversionError` from a failed transformation, become `InvalidData`. `ZipErrors` and `OrErrors` become `And` and `Or`, and `SourceError` becomes `SourceUnavailable`. `Config.Error#prettyPrint` still renders the whole tree. See [Errors](./errors.md).

## Check

Check that the project compiles with no references to `ConfigDescriptor`, `ConfigSource`, `ReadError` or `ZConfig`, and that a test loading each production file with the new descriptions gets the same values as before.

## Related

See [Config descriptors](./config-descriptors.md) for every building block in 4.x, and [What's new since 4.0.6](./whats-new-since-4-0-6.md) for changes within the 4.x series.
