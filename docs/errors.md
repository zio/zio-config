---
id: errors
title: "Errors"
sidebar_label: "Errors"
---

A failed load returns a `Config.Error`. The error describes what went wrong and where: the key path, the expected format and the value that was found. Config errors accumulate. When two keys are wrong, the error reports both, not only the first one.

## Error kinds

`Config.Error` has these cases. Each one carries the key path where the problem occurred:

| Case | When it happens |
| --- | --- |
| `MissingData` | A key with no default or optional wrapper is absent from every source |
| `InvalidData` | A key is present but its value does not match the expected type or predicate |
| `SourceUnavailable` | A source could not be read at all, for example a missing file |
| `Unsupported` | The provider or the description does not support the requested operation |

Handle an error like any ZIO error. For example, log it and fail the startup:

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class Settings(host: String, port: Int)

val load: IO[Config.Error, Settings] =
  ConfigProvider.fromHoconString("host = \"localhost\"").load(deriveConfig[Settings])
```

## Reading an error as text

Call `prettyPrint` on the error to get a readable tree. It shows every problem, grouped by which keys were tried together:

```scala mdoc:compile-only
val reported: UIO[Unit] =
  load.catchAll(error => ZIO.succeed(println(error.prettyPrint()))).unit
```

`prettyPrint` takes an optional `keyDelimiter` (default `.`). Use it when your keys use another separator.

The tree uses these symbols:

- Lines that split into several branches are **and**-errors: all of these problems must be fixed.
- Lines that run one after another are **or**-errors: the first alternative failed and the next one was tried.

For example, a missing `port` that has no default is reported like this:

```text
   ReadError:
   ╥
   ╠─MissingValue
   ║ path: port
   ║ Details: value of type int
   ▼
```

## Accumulating errors

A description built with `zip` or `to` keeps going after a failure. It reads every key and combines the failures:

```scala mdoc:compile-only
final case class Many(a: Int, b: Int, c: String)

val many = (Config.int("A") zip Config.int("B") zip Config.string("C")).to[Many]

val result: IO[Config.Error, Many] =
  ConfigProvider.fromMap(Map("A" -> "x", "B" -> "2")).load(many)
```

`result` fails with an error that lists both problems: `A` has the wrong format, and `C` is missing.

## Next steps

- [Config descriptors](./config-descriptors.md) explains `orElse`, which changes which errors are reported.
- [Validation](./automatic-validations.md) explains how predicate failures appear.
