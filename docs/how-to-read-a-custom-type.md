---
id: how-to-read-a-custom-type
title: "How to Read a Custom Type"
sidebar_label: "Read a Custom Type"
---

Configuration values are strings in the source, but the program wants its own types: a host and port pair, a byte size such as `512MB`, or a percentage between 0 and 100. This guide turns a string, or a group of keys, into such a type and makes it work with derivation.

## Goal

Read `maxUpload = 512MB` into a `ByteSize`, reject invalid values with a clear message, and use `ByteSize` as a field in a derived case class.

## Step 1: Parse the Value With `Config#mapOrFail`

Start from the description of the raw value, here `Config.string`, and convert it. Return `Left` with a `Config.Error.InvalidData` for input you reject, so the error is reported with the key like any other format error:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.{DeriveConfig, deriveConfig}

final case class ByteSize(bytes: Long)

object ByteSize {
  private val Pattern = """(\d+)\s*(B|KB|MB|GB)""".r

  def parse(text: String): Either[Config.Error, ByteSize] =
    text.trim.toUpperCase match {
      case Pattern(n, "B")  => Right(ByteSize(n.toLong))
      case Pattern(n, "KB") => Right(ByteSize(n.toLong * 1024))
      case Pattern(n, "MB") => Right(ByteSize(n.toLong * 1024 * 1024))
      case Pattern(n, "GB") => Right(ByteSize(n.toLong * 1024 * 1024 * 1024))
      case _                => Left(Config.Error.InvalidData(message = s"expected a size such as 512MB, got '$text'"))
    }

  val config: Config[ByteSize] = Config.string.mapOrFail(parse)
}

def load[A](config: Config[A], entries: (String, String)*): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromMap(entries.toMap).load(config).either).getOrThrow()
  }
```

`ByteSize.config` has no key; the caller adds one with `Config#nested`. A valid and an invalid value:

```scala mdoc
load(ByteSize.config.nested("maxUpload"), "maxUpload" -> "512MB")
load(ByteSize.config.nested("maxUpload"), "maxUpload" -> "a lot")
```

If the conversion throws instead of returning an `Either`, use `Config#mapAttempt`; the exception's message becomes the `InvalidData` message. For a check on a value that already has the right type, use `Config#validate`, as in `Config.int.validate("must be 0 to 100")(n => n >= 0 && n <= 100)`.

## Step 2: Make It Work With Derivation

Derivation needs a `DeriveConfig[ByteSize]`. Build it from the description and put it in the companion object, so it is found wherever `ByteSize` is used:

```scala mdoc:silent
object ByteSizeInstances {
  implicit val deriveByteSize: DeriveConfig[ByteSize] = DeriveConfig(ByteSize.config)
}
import ByteSizeInstances._

final case class Upload(maxUpload: ByteSize, directory: String)
```

The derived description reads `maxUpload` with the custom parser:

```scala mdoc
load(deriveConfig[Upload], "maxUpload" -> "2GB", "directory" -> "/tmp/uploads")
```

In your own code, declare `deriveByteSize` in `object ByteSize` itself; this page uses a separate object only because of how the documentation is compiled. On Scala 3, use `DeriveConfig.from(ByteSize.config)`, or declare a `given Config[ByteSize]`, which derivation also accepts.

## Step 3: Read a Type From Several Keys

A type built from several keys is a small description of its own. Combine the keys with `Config#zip` and `Config#to`, then validate the combination:

```scala mdoc:silent
final case class PortRange(from: Int, to: Int)

val portRange: Config[PortRange] =
  (Config.int("from") zip Config.int("to")).to[PortRange]
    .validate("'from' must not be greater than 'to'")(range => range.from <= range.to)
```

The check runs after both keys are read:

```scala mdoc
load(portRange.nested("ports"), "ports.from" -> "9000", "ports.to" -> "8000")
```

The same `DeriveConfig(portRange)` instance lets derivation use it for a field; derivation adds the field name as the prefix.

## Step 4: Describe the Type for Generated Docs

Add a description with `Config#??`, so that [generated documentation](./documentation-and-reporting.md) tells operators the expected format:

```scala mdoc:silent
val documentedSize: Config[ByteSize] = ByteSize.config ?? "a size with a unit: B, KB, MB or GB"
```

## Notes

- Value classes such as `final case class Port(value: Int) extends AnyVal` need no instance on Scala 2; derivation reads them as their field. On Scala 3, write an instance as above.
- For a validation rule that belongs in the type, the refined integration is an alternative; see [Validation](./automatic-validations.md).
- [DeriveConfig reference](./derive-config.md#writing-an-instance) lists the methods for building instances.
