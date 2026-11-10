---
id: how-to-test-config
title: "How to Test Config"
sidebar_label: "Test Config"
---

A config description is a value, so tests can load it from a map instead of the real environment. This guide shows how to write tests that check the settings a program reads and how it reports bad values.

## Goal

Check, without touching the machine's environment or files, that:

1. a complete map loads into the expected value,
2. a missing key falls back to the default,
3. a wrong value fails with a `Config.Error`.

## Step 1: Use a map as the source

`ConfigProvider.fromMap` reads a `Map[String, String]`. It is the same type a real provider produces, so the code under test does not change:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Service(host: String, port: Int, timeoutSeconds: Int)

val serviceDescription: Config[Service] =
  (Config.string("HOST") zip Config.int("PORT") zip Config.int("TIMEOUT").withDefault(30)).to[Service]
```

## Step 2: Check the happy path

```scala mdoc:compile-only
val complete: ConfigProvider =
  ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "8080", "TIMEOUT" -> "5"))

val loadedComplete: IO[Config.Error, Service] = complete.load(serviceDescription)
// Expected: Service("localhost", 8080, 5)
```

## Step 3: Check the default

```scala mdoc:compile-only
val withoutTimeout: ConfigProvider =
  ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "8080"))

val loadedDefault: IO[Config.Error, Service] = withoutTimeout.load(serviceDescription)
// Expected: Service("localhost", 8080, 30)
```

## Step 4: Check the failure

```scala mdoc:compile-only
val badPort: ConfigProvider =
  ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "not-a-number"))

val failed: IO[Config.Error, Service] = badPort.load(serviceDescription)
// Expected: a Config.Error that names PORT
```

In a ZIO Test spec, use `assertZIO(provider.load(desc))(...)` or `.exit` with an `assertTrue` on the result.

## Notes

- Do not use `ConfigProvider.envProvider` in unit tests. A test that depends on the developer's environment is not repeatable.
- For a test that reads a real HOCON file, use `ConfigProvider.fromHoconString` with the file's content as a string.

## Related

- [Config providers](./read-from-various-sources.md) lists the providers you can build from a map or a string.
- [Errors](./errors.md) explains the error values to match on.
