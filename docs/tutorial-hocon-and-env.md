---
id: tutorial-hocon-and-env
title: "Tutorial: Configure an App from HOCON and Environment Variables"
description: "Learn to layer a HOCON defaults file under environment variables and to test the result without the environment."
sidebar_label: "HOCON and Environment Variables"
---

Welcome! This tutorial is for Scala developers who have loaded a first configuration with ZIO Config, for example by following [Getting Started](./getting-started.md). We build the configuration for a small web service. Its defaults live in a HOCON file in the repository, and anything set in an environment variable overrides the file. This is the pattern most services use: defaults in version control, and per-machine values from the environment.

You need Scala 2.13 or Scala 3, sbt, and these dependencies:

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-config"          % "@VERSION@",
  "dev.zio" %% "zio-config-typesafe" % "@VERSION@",
  "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
)
```

## Learning Objectives

By the end of this tutorial, you will understand:

- How to derive a configuration description from a case class and adjust its key names
- How to layer a HOCON defaults file under environment variables
- How to replace the environment in tests

## Step 1: Describe the Settings

Start with the settings the service needs. Each field is one value the program must have. Write the case class first, so the rest of the tutorial has a fixed target:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class Service(
  host: String,
  port: Int,
  databaseUrl: String,
  logLevel: String
)
```

The field names are the keys the library looks for. For this tutorial, the keys are written in upper snake case, as environment variables usually are: `HOST`, `PORT`, `DATABASE_URL` and `LOG_LEVEL`.

## Step 2: Write the Description

Turn the case class into a `Config` with `deriveConfig`. Then change every key with `Config#mapKey`: `toSnakeCase` turns `databaseUrl` into `database_url`, and `toUpperCase` makes it `DATABASE_URL`:

```scala mdoc:silent
val serviceDescription: Config[Service] =
  deriveConfig[Service].mapKey(key => toSnakeCase(key).toUpperCase)
```

`Config#mapKey` changes every key in the description: `host` becomes `HOST`, `port` becomes `PORT`, and the two camelCase names get an underscore.

## Step 3: Write the Defaults File

The defaults go into a HOCON string. In a real project, put this text in `src/main/resources/application.conf` and load it with `ConfigProvider.fromResourcePath()`. This tutorial keeps it in a string so the code is self-contained:

```scala mdoc:silent
val defaultsHocon: String =
  """
  HOST = "0.0.0.0"
  PORT = 8080
  DATABASE_URL = "postgres://localhost:5432/orders"
  LOG_LEVEL = "info"
  """

val defaults: ConfigProvider = ConfigProvider.fromHoconString(defaultsHocon)
```

## Step 4: Read the Environment

ZIO provides a provider that reads the process environment. Use it for values that change per machine:

```scala mdoc:silent
val environment: ConfigProvider = ConfigProvider.envProvider
```

## Step 5: Layer the Two Sources

`ConfigProvider#orElse` tries the first provider for each key, and falls back to the second one when the first can't supply the key. Put the environment first, so its values win, and the defaults second:

```scala mdoc:silent
val layered: ConfigProvider = environment.orElse(defaults)
```

With `layered`, a key set in the environment is read from there. Any other key comes from the HOCON defaults. You can see this with the `PORT` key in Step 6.

## Step 6: Load and Run

Put the pieces into a program. The program loads the config, prints it and fails if a required key is missing:

```scala mdoc:compile-only
object OrdersService extends ZIOAppDefault {

  val load: IO[Config.Error, Service] =
    ConfigProvider.envProvider
      .orElse(ConfigProvider.fromHoconString(defaultsHocon))
      .load(serviceDescription)

  def run =
    for {
      config <- load
      _      <- Console.printLine(s"listening on ${config.host}:${config.port}, logging at ${config.logLevel}")
    } yield ()
}
```

Run it with the defaults:

```text
sbt run
listening on 0.0.0.0:8080, logging at info
```

Run it again with an override:

```text
PORT=9090 LOG_LEVEL=debug sbt run
listening on 0.0.0.0:9090, logging at debug
```

The environment supplied `PORT` and `LOG_LEVEL`, and the HOCON file supplied `HOST` and `DATABASE_URL`.

## Step 7: Test It Without the Environment

Tests should not depend on what is set on the machine. Replace the environment with a map that holds the values under test. The rest of the description is unchanged:

```scala mdoc:compile-only
val testSource: ConfigProvider =
  ConfigProvider
    .fromMap(Map("PORT" -> "9999"))
    .orElse(ConfigProvider.fromHoconString(defaultsHocon))

val testLoaded: IO[Config.Error, Service] = testSource.load(serviceDescription)
```

`testLoaded` reads `PORT` as 9999 and everything else from the defaults. See [How to test config](./how-to-test-config.md) for a full test setup.

## What You've Learned

- A case class that states the settings.
- One description derived from it, with the key names adjusted.
- A defaults file and an environment provider, layered so the environment wins.
- A program that loads the layered config and fails with a `Config.Error` when a required value is missing or invalid.

## Where to Go Next

- [How to read secrets](./how-to-read-secrets.md) covers values that must not appear in a file.
- [How to override settings per environment](./how-to-override-per-environment.md) shows how to use one file per deployment.
- [Errors](./errors.md) explains what the program prints when a value is missing or has the wrong type.
