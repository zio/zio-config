---
id: tutorial-hocon-and-env
title: "Tutorial: Configure an App from HOCON and Environment Variables"
description: "Learn to layer a HOCON defaults file under environment variables and to test the result without the environment."
sidebar_label: "HOCON and Environment Variables"
---

Welcome! This tutorial is for Scala developers who have loaded a first configuration with ZIO Config, for example by following [Getting Started](./getting-started.md). We build the configuration for a small web service. Its defaults live in a HOCON file in the repository, and anything set in an environment variable overrides the file. This is the pattern most services use: defaults in version control, and per-machine values from the environment.

We need Scala 2.13 or Scala 3, sbt, and these dependencies:

```scala
// ZIO Config core: config descriptors and readers (JVM)
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"

// HOCON and JSON sources, built on Typesafe Config (JVM)
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"

// Automatic derivation of config descriptors (JVM)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

## Learning Objectives

By the end of this tutorial, you will understand:

- How to derive a configuration description from a case class and adjust its key names
- How to layer a HOCON defaults file under environment variables
- How to replace the environment in tests

We'll learn these concepts through:

1. Describing the settings
2. Writing the description
3. Writing the defaults file
4. Reading the environment
5. Layering the two sources
6. Loading and running
7. Testing without the environment

We recommend reading from top to bottom — each step builds on the previous one.

## Step 1: Describe the Settings

Let's start with the settings the service needs. Each field is one value the program must have. We write the case class first, so the rest of the tutorial has a fixed target. We also add a small helper, `load`, that runs a load and returns the error or the value, so we can see the result of each step:

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

def load[A](provider: ConfigProvider, config: Config[A]): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(provider.load(config).either).getOrThrow()
  }
```

The field names are the keys the library looks for. For this tutorial, we write the keys in upper snake case, as environment variables usually are: `HOST`, `PORT`, `DATABASE_URL` and `LOG_LEVEL`.

## Step 2: Write the Description

Now we turn the case class into a `Config` with `deriveConfig`, and change every key with `Config#mapKey`: `toSnakeCase` turns `databaseUrl` into `database_url`, and `toUpperCase` makes it `DATABASE_URL`:

```scala mdoc:silent
val serviceDescription: Config[Service] =
  deriveConfig[Service].mapKey(key => toSnakeCase(key).toUpperCase)
```

Notice that `Config#mapKey` changes every key in the description at once: `host` becomes `HOST`, `port` becomes `PORT`, and the two camelCase names get an underscore.

## Step 3: Write the Defaults File

The defaults go into a HOCON document. In a real project, this text lives in `src/main/resources/application.conf` and `ConfigProvider.fromResourcePath()` loads it. Here we keep it in a string so the code is self-contained:

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

Loading the description from the defaults alone already gives a complete `Service`:

```scala mdoc
load(defaults, serviceDescription)
```

## Step 4: Read the Environment

ZIO provides a provider that reads the process environment, `ConfigProvider.envProvider`. We use it for the values that change per machine:

```scala mdoc:silent
val environment: ConfigProvider = ConfigProvider.envProvider
```

The environment provider looks up a key such as `PORT` as the variable `PORT`. These docs can't set real environment variables, so for the rest of the tutorial a map stands in for an environment where `PORT` and `LOG_LEVEL` are set:

```scala mdoc:silent
val exampleEnvironment: ConfigProvider =
  ConfigProvider.fromMap(Map("PORT" -> "9090", "LOG_LEVEL" -> "debug"))
```

## Step 5: Layer the Two Sources

`ConfigProvider#orElse` tries the first provider for each key, and falls back to the second one when the first can't supply the key. We put the environment first, so its values win, and the defaults second:

```scala mdoc:silent
val layered: ConfigProvider = environment.orElse(defaults)
```

Let's see the layering at work with the example environment in place of the real one:

```scala mdoc
load(exampleEnvironment.orElse(defaults), serviceDescription)
```

Notice where each value came from: `PORT` and `LOG_LEVEL` from the environment, `HOST` and `DATABASE_URL` from the HOCON defaults.

## Step 6: Load and Run

Now we put the pieces into a program. It loads the config, prints it, and fails with a `Config.Error` if a required key is missing or invalid:

```scala mdoc:compile-only
object OrdersService extends ZIOAppDefault {

  def run =
    for {
      config <- layered.load(serviceDescription)
      _      <- Console.printLine(s"listening on ${config.host}:${config.port}, logging at ${config.logLevel}")
    } yield ()
}
```

Running it with no variables set prints the defaults:

```text
sbt run
listening on 0.0.0.0:8080, logging at info
```

Try running it again with an override, and the environment wins for those two keys:

```text
PORT=9090 LOG_LEVEL=debug sbt run
listening on 0.0.0.0:9090, logging at debug
```

## Step 7: Test It Without the Environment

Tests should not depend on what is set on the machine. In a test, we replace the environment with a map that holds the values under test; the description and the defaults stay unchanged:

```scala mdoc:silent
val testSource: ConfigProvider =
  ConfigProvider.fromMap(Map("PORT" -> "9999")).orElse(defaults)
```

The test source gives `PORT` as 9999 and everything else from the defaults:

```scala mdoc
load(testSource, serviceDescription)
```

## What You've Learned

In this tutorial, you learned:

- How `deriveConfig` turns a case class into a description, and how `Config#mapKey` adapts its key names to the source
- How `ConfigProvider#orElse` layers sources, so that the environment overrides a HOCON defaults file key by key
- How to swap the environment for a map in tests, without changing the description

You now know the pattern most services use for their configuration. The how-to guides show how to apply it to specific situations.

## Where to Go Next

- **Need values that must not be committed?** [How to read secrets](./how-to-read-secrets.md) covers passwords and API keys.
- **One file per deployment?** [How to override settings per environment](./how-to-override-per-environment.md) selects a file with an environment variable.
- **Want to understand failures?** [Errors](./errors.md) explains what the program prints when a value is missing or has the wrong type.
