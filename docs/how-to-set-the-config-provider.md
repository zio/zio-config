---
id: how-to-set-the-config-provider
title: "How to Set the Config Provider for a Whole Application"
sidebar_label: "Set the Provider for an Application"
---

Passing a `ConfigProvider` through a program by hand gets tedious. ZIO keeps one provider in the runtime, and any code can read from it with `ZIO.config`. This guide sets that provider once, at startup, from a HOCON file with environment overrides, and swaps it in tests.

## Goal

Read settings anywhere with `ZIO.config`, take them from `application.conf` with environment variables winning, and give tests their own values without touching the environment.

## Step 1: Describe the Settings Next to the Type

Put the description in the companion object, so every reader uses the same one. Making it `implicit` lets `ZIO.config[A]` find it:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class HttpConfig(host: String, port: Int)

object HttpConfig {
  implicit val config: Config[HttpConfig] = deriveConfig[HttpConfig].nested("http")
}
```

## Step 2: Build the Provider in `bootstrap`

`Runtime.setConfigProvider` returns a layer that replaces the runtime's provider. Put it in the `bootstrap` of your `ZIOAppDefault`, so it is in place before `run` starts. Layer the sources with `ConfigProvider#orElse`, the one that wins first:

```scala mdoc:silent
object Providers {
  val application: ConfigProvider =
    ConfigProvider.envProvider.upperCase.orElse(ConfigProvider.fromResourcePath())
}
```

`ConfigProvider#upperCase` makes the environment provider look up `HTTP_PORT` for the key `http.port`, so an exported `HTTP_PORT` overrides the file. The app sets it in `bootstrap` and reads with `ZIO.config`:

```scala mdoc:compile-only
object Main extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(Providers.application)

  def run =
    for {
      http <- ZIO.config[HttpConfig]
      _    <- Console.printLine(s"Listening on ${http.host}:${http.port}")
    } yield ()
}
```

If building the provider can fail, for example with `ConfigProvider.fromHoconFileZIO`, build it in a layer and pass the result on: `ZLayer.fromZIO(ConfigProvider.fromHoconFileZIO(file)).flatMap(env => Runtime.setConfigProvider(env.get))`.

## Step 3: Hand Settings to Services as Layers

A service that needs settings can take them as a dependency. `ZLayer.fromZIO(ZIO.config[HttpConfig])` reads them when the layer is built:

```scala mdoc:silent
final class HttpServer(config: HttpConfig) {
  def describe: String = s"server on ${config.host}:${config.port}"
}

object HttpServer {
  val layer: ZLayer[Any, Config.Error, HttpServer] =
    ZLayer.fromZIO(ZIO.config[HttpConfig]).map(env => ZEnvironment(new HttpServer(env.get)))
}
```

A missing or invalid setting then fails the layer, and the application stops at startup with the `Config.Error`.

## Step 4: Use Other Values in Tests

`ZIO.withConfigProvider` runs an effect with another provider. Tests use it to supply exact values:

```scala mdoc:silent
val testProvider = ConfigProvider.fromMap(Map("http.host" -> "localhost", "http.port" -> "0"))
```

Running the service's layer with `testProvider` gives the test values:

```scala mdoc
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(
    ZIO.withConfigProvider(testProvider)(ZIO.serviceWith[HttpServer](_.describe).provide(HttpServer.layer))
  ).getOrThrow()
}
```

In a ZIO Test suite, provide `Runtime.setConfigProvider(testProvider)` as a layer with `Spec#provideLayer` or `Spec#provide` to do the same for a whole suite.

## Notes

- Without `Runtime.setConfigProvider`, `ZIO.config` uses `ConfigProvider.defaultProvider`: environment variables, then system properties.
- Use `ConfigProvider#load` instead of `ZIO.config` when a single part of a program needs a provider of its own.
- [ZIO core providers](./zio-core-providers.md) lists the providers and the combinators used here.
