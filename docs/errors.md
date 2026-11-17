---
id: errors
title: "Errors"
sidebar_label: "Errors"
---

A failed load returns a `Config.Error`. The error says what went wrong and where: the key path, and a message such as the expected type and the value that was found. Config errors accumulate. When two keys are wrong, the error reports both, not only the first one.

`Config.Error` is defined in ZIO itself. ZIO Config adds `Config.Error#prettyPrint`, which renders an error as a readable tree.

## Error Cases

`Config.Error` is a sealed trait with six cases. Four of them describe one problem and carry the path of the key. The other two combine errors:

| Case                | Carries                        | When it happens                                                                                                             |
| ------------------- | ------------------------------ | --------------------------------------------------------------------------------------------------------------------------- |
| `MissingData`       | `path`, `message`              | A key is absent, and the description has no `Config#optional` or `Config#withDefault` for it                                |
| `InvalidData`       | `path`, `message`              | A key is present, but its value can't be parsed, or `Config#validate`, `Config#mapOrFail` or a refined predicate rejects it |
| `SourceUnavailable` | `path`, `message`, `cause`     | A ZIO core provider can't read its source, for example the environment, the system properties or the console               |
| `Unsupported`       | `path`, `message`              | An operation is not supported; ZIO Config also uses it for exceptions while building a provider, such as an AWS failure     |
| `And`               | `left`, `right`                | Both sides of a `Config#zip` failed; both problems need fixing                                                              |
| `Or`                | `left`, `right`                | Both sides of a `Config#orElse` failed; either side could be fixed                                                          |

A provider built from a file is different. `ConfigProvider.fromHoconFile`, `ConfigProvider.fromYamlFile` and the other file-based constructors read the file when you call them, and they throw if it is malformed (YAML and TOML also throw for a missing file; a missing HOCON file reads as empty). Their `...ZIO` variants, such as `ConfigProvider.fromHoconFileZIO`, return a `Task[ConfigProvider]` that fails with the exception. Neither of them produces a `Config.Error`.

The examples below share these imports and a helper, `report`, that loads a description from a map and prints the pretty-printed error:

```scala mdoc:silent
import zio._
import zio.config._

def report[A](config: Config[A], entries: (String, String)*): Unit =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromMap(entries.toMap).load(config).either).getOrThrow() match {
      case Left(error)  => println(error.prettyPrint())
      case Right(value) => println(s"Loaded: $value")
    }
  }
```

## Reading an Error as Text

`Config.Error#prettyPrint` turns an error into text. For a single problem, it prints the kind of problem, the path and the message:

```scala mdoc
report(Config.int("port"))
```

A wrong value prints a `FormatError` with the cause:

```scala mdoc
report(Config.int("port"), "port" -> "eighty")
```

`Config.Error#prettyPrint` takes an optional `keyDelimiter` (default `'.'`) that joins the segments of a path. Pass another character when your keys use another separator, for example `error.prettyPrint('_')` for environment variables.

The labels map to the cases this way: `MissingValue` is `MissingData`, `FormatError` is `InvalidData`, `ConversionError` is `Unsupported`, and `SourceError` is `SourceUnavailable`.

## Accumulating Errors

A description built with `Config#zip` or `Config#to` reads every key, even after one of them fails, and joins the failures with `And`. `Config.Error#prettyPrint` shows an `And` as parallel branches, one column per problem:

```scala mdoc:silent
final case class Many(a: Int, b: Int, c: String)

val many: Config[Many] = (Config.int("A") zip Config.int("B") zip Config.string("C")).to[Many]
```

Loading `many` with a wrong `A` and no `C` reports both problems:

```scala mdoc
report(many, "A" -> "x", "B" -> "2")
```

## Errors From Alternatives

`Config#orElse` tries the second description only when the first one fails. When both fail, the error is an `Or`, and `Config.Error#prettyPrint` shows its parts one after another, in the order they were tried:

```scala mdoc:silent
val databaseUrl: Config[String] =
  Config.string("DATABASE_URL") orElse Config.string("DB_URL")
```

With neither key set, the report lists both attempts:

```scala mdoc
report(databaseUrl)
```

## Defaults Apply Only to Missing Data

`Config#withDefault` and `Config#optional` cover a missing key, not a wrong one. A value that fails to parse still fails the load, so a typo in a setting is reported instead of being replaced by the default:

```scala mdoc
report(Config.int("TIMEOUT").withDefault(30))
report(Config.int("TIMEOUT").withDefault(30), "TIMEOUT" -> "thirty")
```

`Config.Error#isMissingDataOnly` is the check behind this rule. It is `true` when every problem inside the error is a `MissingData`.

## Handling an Error

`Config.Error` is an ordinary ZIO error. Handle it at startup with `ZIO#catchAll`, `ZIO#mapError` or `ZIO#tapError`, for example to log the pretty-printed report and exit:

```scala mdoc:compile-only
final case class Settings(host: String, port: Int)

val settings: Config[Settings] = (Config.string("host") zip Config.int("port")).to[Settings]

val load: IO[Config.Error, Settings] =
  ConfigProvider.fromMap(Map("host" -> "localhost")).load(settings)

val startup: Task[Settings] =
  load.tapError(error => ZIO.logError(error.prettyPrint())).mapError(error => new RuntimeException(error.getMessage))
```

To branch on the kind of problem, match on the cases or use `Config.Error#fold`, which takes one function per case:

```scala mdoc:compile-only
def describe(error: Config.Error): String =
  error match {
    case Config.Error.MissingData(path, _)    => s"missing ${path.mkString(".")}"
    case Config.Error.InvalidData(path, msg)  => s"invalid ${path.mkString(".")}: $msg"
    case Config.Error.And(left, right)        => s"${describe(left)} and ${describe(right)}"
    case Config.Error.Or(left, right)         => s"${describe(left)} or ${describe(right)}"
    case other                                => other.getMessage
  }
```

`Config.Error` also extends `Exception`, so it can be thrown or wrapped where a `Throwable` is expected. Its `getMessage` is a one-line form such as `(Missing data at port: Expected port to be set in properties)`, and an `And` or `Or` joins its parts with ` and ` or ` or `.

## Next Steps

- [Config descriptors](./config-descriptors.md) explains `Config#orElse`, `Config#withDefault` and `Config#validate`, which shape the errors.
- [Validation](./automatic-validations.md) explains how a failed refined predicate appears.
- [How to test config](./how-to-test-config.md) shows how to assert on an error in a test.
