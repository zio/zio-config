---
id: getting-started
title: "Getting Started"
sidebar_label: "Getting Started"
---

This page takes you from an empty sbt project to a running program that reads its settings from a HOCON file. You will add three dependencies, describe a configuration with a case class, load it, and run it.

## Terms used on this page

- **Config description**: a value of type `Config[A]` that says which keys to read and how to turn them into an `A`. It does not read anything by itself.
- **Config provider**: a value of type `ConfigProvider` that holds the raw key-value data. It is built from a source such as a HOCON string, a YAML file or a map. You give a config description to a provider to load it.
- **Derivation**: generating a config description from a case class automatically, instead of writing it by hand.

## 1. Add the dependencies

Add these lines to `build.sbt`. Replace `<version>` with the latest release.

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-config"          % "<version>",
  "dev.zio" %% "zio-config-typesafe" % "<version>", // HOCON and JSON sources
  "dev.zio" %% "zio-config-magnolia" % "<version>"  // automatic derivation
)
```

- `zio-config` holds the core API.
- `zio-config-typesafe` adds `ConfigProvider.fromHoconString` and related methods.
- `zio-config-magnolia` adds `deriveConfig`, which builds a config description from a case class.

Other sources (YAML, TOML, XML, AWS Parameter Store) use their own modules. See [Config providers](./read-from-various-sources.md).

## 2. Describe the configuration

Define a case class for the settings you need, and derive its config description:

```scala mdoc:silent
import zio.config.magnolia.deriveConfig

final case class AppConfig(host: String, port: Int)

val appConfigDescription = deriveConfig[AppConfig]
```

`deriveConfig` looks at the field names (`host`, `port`) and their types. It uses them as the keys to read, so the source must contain `host` and `port`.

## 3. Load it from a source

Put the settings in a HOCON string, build a provider from it, and load the description:

```scala mdoc:compile-only
import zio._
import zio.config.typesafe._

val settings: String =
  """
  host = "localhost"
  port = 8080
  """

val provider: ConfigProvider = ConfigProvider.fromHoconString(settings)

val loaded: IO[Config.Error, AppConfig] = provider.load(appConfigDescription)
```

`provider.load(...)` returns a `ZIO`. Nothing is read until that effect runs. If a key is missing or has the wrong type, the effect fails with a `Config.Error`. See [Errors](./errors.md).

## 4. Run it

Wrap the load in a `ZIOAppDefault`, print the result, and run the program:

```scala mdoc:compile-only
import zio._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

object Main extends ZIOAppDefault {

  final case class AppConfig(host: String, port: Int)

  val settings: String =
    """
    host = "localhost"
    port = 8080
    """

  def run =
    for {
      config <- ConfigProvider.fromHoconString(settings).load(deriveConfig[AppConfig])
      _      <- Console.printLine(s"Starting on ${config.host}:${config.port}")
    } yield ()
}
```

Running `Main` prints `Starting on localhost:8080`.

## Where to go next

- [Config descriptors](./config-descriptors.md) explains the building blocks: `string`, `int`, `zip`, `nested`, `optional`, `withDefault`.
- [Derivation](./automatic-derivation-of-config.md) explains how names are mapped to keys and how to rename them.
- [Config providers](./read-from-various-sources.md) lists every source.
- [Tutorial: Configure an app from HOCON and environment variables](./tutorial-hocon-and-env.md) builds a realistic setup step by step.
