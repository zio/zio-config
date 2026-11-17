---
id: tutorial-config-in-a-zio-app
title: "Tutorial: Load Configuration in a ZIO Application"
sidebar_label: "Configuration in a ZIO App"
description: "Learn how a ZIO application reads its configuration with ZIO.config, sets its source at startup, and passes settings to services as layers."
keywords:
  - "Configuration"
  - "ZIO Application"
  - "ZLayer"
  - "ZIO.config"
---

Welcome! This tutorial is for Scala developers who know the basics of ZIO — effects, `ZIOAppDefault` and layers — and who have described a configuration with ZIO Config, for example in [Tutorial: Model a Real Configuration](./tutorial-model-config-with-adts.md). You don't need to know how ZIO handles configuration internally.

## Learning Objectives

By the end of this tutorial, you will understand:

- Where a ZIO application gets its configuration from, and how `ZIO.config` reads it
- How to choose the configuration source once, when the application starts
- How to hand settings to services through layers
- How to report a bad configuration clearly and how to replace the source in tests

We'll learn these concepts through:

1. Reading configuration with `ZIO.config`
2. Choosing the source at startup
3. Settings as a layer
4. Failing fast with a clear message
5. Replacing the source in tests

We recommend reading from top to bottom — each section builds on the previous one.

## Background

Every ZIO runtime holds one `ConfigProvider`, the source of configuration for the whole program. Out of the box it reads environment variables and system properties. `ZIO.config(description)` is an effect that loads a description from that provider. Because the provider lives in the runtime, code that needs settings doesn't take a provider as a parameter: it asks for the settings, and the application decides once, at startup, where they come from.

We build a small greeting service that needs a host, a port and a greeting text.

## 1. Reading Configuration With `ZIO.config`

Let's describe the settings and read them with `ZIO.config`. We put the description in the companion object as an `implicit`, so `ZIO.config[ServerConfig]` can find it by type:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class ServerConfig(host: String, port: Int, greeting: String)

object ServerConfig {
  implicit val config: Config[ServerConfig] = deriveConfig[ServerConfig].nested("server")
}

val readServer: IO[Config.Error, ServerConfig] = ZIO.config[ServerConfig]
```

The code above has three parts:

- `ServerConfig` lists the settings the service needs.
- `ServerConfig.config` derives the description and puts every key under `server`, so the keys are `server.host`, `server.port` and `server.greeting`.
- `readServer` is an effect that, when it runs, loads the description from the runtime's provider.

To see `readServer` work, we run it with a provider built from a HOCON string. `ZIO.withConfigProvider` replaces the provider for one effect, and we wrap the run in a small helper that returns the error or the value:

```scala mdoc:silent
val settings: String =
  """
  server {
    host = "0.0.0.0"
    port = 8080
    greeting = "Hello"
  }
  """

def runWith[A](provider: ConfigProvider)(effect: ZIO[Any, Any, A]): Either[Any, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ZIO.withConfigProvider(provider)(effect).either).getOrThrow()
  }
```

Running `readServer` with that provider gives the settings:

```scala mdoc
runWith(ConfigProvider.fromHoconString(settings))(readServer)
```

Notice that `readServer` never mentions HOCON. The same effect would read environment variables if we ran it with another provider.

## 2. Choosing the Source at Startup

In a real application, we don't wrap every effect. Instead, we set the provider once, in the `bootstrap` layer of `ZIOAppDefault`, with `Runtime.setConfigProvider`. Here the source is the HOCON text, with environment variables taking priority:

```scala mdoc:compile-only
object GreetingApp extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(
      ConfigProvider.envProvider.orElse(ConfigProvider.fromHoconString(settings))
    )

  def run =
    readServer.flatMap(config => Console.printLine(s"Listening on ${config.host}:${config.port}"))
}
```

The code above sets the source for the whole program:

- `bootstrap` runs before `run`, so the provider is in place when `readServer` runs.
- `ConfigProvider.envProvider` joins the path with `_` and upper-cases it, so it reads `SERVER_PORT` for the key `server.port`.
- `ConfigProvider#orElse` falls back to the HOCON text for every key the environment doesn't supply.

Running the app prints `Listening on 0.0.0.0:8080`; running it with `SERVER_PORT=9090` prints port 9090. In a real project, the HOCON text lives in `application.conf`, and `ConfigProvider.fromResourcePath()` reads it.

## 3. Settings as a Layer

Services shouldn't read configuration themselves; they should receive it. A layer built with `ZLayer.fromZIO(ZIO.config[...])` turns the settings into a dependency that other layers use:

```scala mdoc:silent
final class Greeter(config: ServerConfig) {
  def greet(name: String): String = s"${config.greeting}, $name!"
}

object Greeter {
  val layer: ZLayer[Any, Config.Error, Greeter] =
    ZLayer.fromZIO(ZIO.config[ServerConfig].map(new Greeter(_)))
}
```

In the code above, `Greeter` takes its settings as a constructor argument and knows nothing about configuration. `Greeter.layer` reads the settings when the layer is built and creates the service.

A program that uses the service only asks for a `Greeter`:

```scala mdoc:silent
val greetAda: ZIO[Greeter, Nothing, String] = ZIO.serviceWith[Greeter](_.greet("Ada"))
```

Providing `Greeter.layer` builds the service from the settings:

```scala mdoc
runWith(ConfigProvider.fromHoconString(settings))(greetAda.provide(Greeter.layer))
```

## 4. Failing Fast With a Clear Message

When a setting is missing or invalid, the layer fails with a `Config.Error`, and the application stops before it does any work. That is what we want, but the default error output is hard to read. `Config.Error#prettyPrint` renders every problem at once, so we log it before the application stops:

```scala mdoc:silent
val reported: ZIO[Any, Any, String] =
  greetAda.provide(Greeter.layer).tapError {
    case error: Config.Error => ZIO.succeed(println(error.prettyPrint()))
    case _                   => ZIO.unit
  }
```

With a document where `port` is not a number and `greeting` is missing, the report lists both problems:

```scala mdoc
runWith(ConfigProvider.fromHoconString("""server { host = "0.0.0.0", port = "eighty" }"""))(reported)
```

## 5. Replacing the Source in Tests

Because the source is chosen in one place, a test can replace it without touching the environment. `ZIO.withConfigProvider` does that for one effect; with ZIO Test, provide `Runtime.setConfigProvider(...)` as a layer to the whole suite:

```scala mdoc:silent
val testProvider: ConfigProvider =
  ConfigProvider.fromMap(Map("server.host" -> "localhost", "server.port" -> "0", "server.greeting" -> "Hi"))
```

With the test values, the greeting changes:

```scala mdoc
runWith(testProvider)(greetAda.provide(Greeter.layer))
```

The service code and the layer are unchanged; only the provider differs.

## Putting It Together

This example combines all the concepts into one application: the description in a companion object, the source chosen in `bootstrap`, the settings handed to `Greeter` as a layer, and a clear report when loading fails:

```scala mdoc:compile-only
object GreetingService extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(
      ConfigProvider.envProvider.orElse(ConfigProvider.fromResourcePath())
    )

  def run =
    ZIO
      .serviceWithZIO[Greeter](greeter => Console.printLine(greeter.greet("world")))
      .provide(Greeter.layer)
      .tapError {
        case error: Config.Error => ZIO.logError(error.prettyPrint())
        case _                   => ZIO.unit
      }
}
```

## What You've Learned

In this tutorial, you learned:

- That the ZIO runtime holds one `ConfigProvider`, and `ZIO.config` loads a description from it
- How `Runtime.setConfigProvider` in `bootstrap` chooses the source once, for the whole application
- How `ZLayer.fromZIO(ZIO.config[...])` hands settings to services as a dependency
- How `Config.Error#prettyPrint` reports every problem at startup, and how `ZIO.withConfigProvider` replaces the source in tests

You now know how configuration flows through a ZIO application. The how-to guides show how to apply this to specific setups.

## Where to Go Next

- **Setting up a real project?** [How to set the config provider for a whole application](./how-to-set-the-config-provider.md) covers layered files and failing providers.
- **Different settings per deployment?** [How to override settings per environment](./how-to-override-per-environment.md) uses one file per environment.
- **All the providers ZIO offers?** See [ZIO core providers](./zio-core-providers.md).
