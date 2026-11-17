---
id: derive-config
title: "DeriveConfig"
sidebar_label: "DeriveConfig"
---

`DeriveConfig[A]` is the type class behind [derivation](./automatic-derivation-of-config.md). It holds a `Config[A]`, and `deriveConfig[A]` returns that description. The `zio-config-magnolia` module provides instances for common types and derives instances for case classes and sealed traits at compile time.

You need `DeriveConfig` directly in two cases: to give derivation an instance for a type it can't derive, and to look up which types work out of the box.

## Definition

`DeriveConfig` is a case class that wraps a description. On Scala 2 it also records whether the type is a case object:

```scala
final case class DeriveConfig[T](desc: Config[T], isObject: Boolean = false, constValue: Option[T] = None)
```

On Scala 3 it carries metadata about names instead:

```scala
final case class DeriveConfig[A](desc: Config[A], metadata: Option[DeriveConfig.Metadata] = None)
```

The extra fields are used by derivation itself. Code that builds an instance uses `DeriveConfig.apply`, the transformation methods below, or (on Scala 3) `DeriveConfig.from`.

## Getting a Description

The examples on this page use these imports:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia._

final case class Endpoint(host: String, port: Int)
```

`deriveConfig[A]` summons the instance and returns its description. `DeriveConfig[A]` returns the instance itself, and `DeriveConfig#desc` is its description:

```scala mdoc:silent
val viaFunction: Config[Endpoint]          = deriveConfig[Endpoint]
val instance: DeriveConfig[Endpoint]       = DeriveConfig[Endpoint]
val viaInstance: Config[Endpoint]          = instance.desc
```

## Built-in Instances

`DeriveConfig` has instances for these types. Each one uses the ZIO Config description in the second column:

| Type                                      | Description used                     |
| ----------------------------------------- | ------------------------------------ |
| `String`                                  | `Config.string`                      |
| `Boolean`                                 | `Config.boolean`                     |
| `Byte`, `Short`, `Int`, `Long`            | `Config.byte`, `Config.short`, `Config.int`, `Config.long` |
| `BigInt`                                  | `Config.bigInt`                      |
| `Float`, `Double`, `BigDecimal`           | `Config.float`, `Config.double`, `Config.bigDecimal` |
| `java.util.UUID`                          | `Config.uuid`                        |
| `java.net.URI`                            | `Config.uri`                         |
| `zio.Duration`                            | `Config.duration`                    |
| `java.time.LocalDate`, `LocalTime`, `LocalDateTime`, `OffsetDateTime` | `Config.localDate`, `Config.localTime`, `Config.localDateTime`, `Config.offsetDateTime` |
| `zio.LogLevel`                            | `Config.logLevel`                    |
| `zio.Config.Secret`                       | `Config.secret`                      |
| `Option[A]`                               | `Config#optional`                    |
| `Either[A, B]`                            | `Config#orElseEither`: tries `A`, then `B` |
| `List[A]`, `Seq[A]`                       | `Config.listOf`                      |
| `Vector[A]`, `Set[A]`                     | `Config.vectorOf`, `Config.setOf`    |
| `Chunk[A]`, `NonEmptyChunk[A]`            | `Config.chunkOf`, `Config.nonEmptyChunkOf` |
| `Map[String, A]`                          | `Config.table`                       |

The collection and `Option` instances need an instance for the element type. With `zio-config-refined`, `Refined[A, P]` also has an instance when `A` has one; see [Validation](./automatic-validations.md).

## How Types Become Descriptions

Derivation follows fixed rules for case classes and sealed traits:

| Scala shape                               | Description produced                                                                      |
| ----------------------------------------- | ----------------------------------------------------------------------------------------- |
| Case class with fields                    | Every field read with `Config#nested(fieldName)` and combined; a field is required unless its type is `Option` or it has a default value |
| Field with a default value                | That field's description with `Config#withDefault(default)`                                |
| Case object, or case class without fields | `Config.constant(name)`: the value at the current path must equal the name                |
| Sealed trait                              | One alternative per subtype, joined with `Config#orElse`; subtypes with fields sit under `Config#nested(subtypeName)` |
| Sealed trait with `@discriminator(key)`   | `Config.string(key)` followed by `Config#switch` over the subtype names                   |
| Value class (Scala 2 only)                | The description of its single field, without an extra key                                 |

The annotations that change the names are described on the [Derivation](./automatic-derivation-of-config.md) page.

## Writing an Instance

To support a type that has no instance, transform an existing instance. The methods mirror the ones on `Config`:

| Method                      | Use it when                                                                     |
| --------------------------- | ------------------------------------------------------------------------------- |
| `DeriveConfig#map`          | The conversion always succeeds                                                  |
| `DeriveConfig#mapAttempt`   | The conversion can throw; the exception becomes an `InvalidData` error          |
| `DeriveConfig#mapOrFail`    | The conversion returns `Either[Config.Error, B]`                                |
| `DeriveConfig#describe`, `DeriveConfig#??` | You add a description for documentation generation              |

To read a `java.nio.file.Path` from a string, with a description:

```scala mdoc:silent
import java.nio.file.{Path, Paths}

implicit val derivePath: DeriveConfig[Path] =
  DeriveConfig[String].mapAttempt(Paths.get(_)) ?? "a file system path"

final case class Storage(dataDir: Path, maxFiles: Int)
```

With that instance in scope, `Storage` derives as usual:

```scala mdoc
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(
    ConfigProvider.fromMap(Map("dataDir" -> "/var/lib/app", "maxFiles" -> "100")).load(deriveConfig[Storage])
  ).getOrThrow()
}
```

To build an instance from a hand-written description, wrap it with `DeriveConfig.apply` on Scala 2, or `DeriveConfig.from` on Scala 3:

```scala mdoc:silent
final case class HostPort(host: String, port: Int)

implicit val deriveHostPort: DeriveConfig[HostPort] =
  DeriveConfig(Config.string.mapOrFail { text =>
    text.split(":") match {
      case Array(host, port) if port.forall(_.isDigit) => Right(HostPort(host, port.toInt))
      case _                                           => Left(Config.Error.InvalidData(message = s"expected host:port, got $text"))
    }
  })
```

A field of type `HostPort` now reads one value such as `db.internal:5432`. Leave the key out of the hand-written description (`Config.string`, not `Config.string("name")`): derivation adds the field name.

A case class with a `HostPort` field reads it from a single key:

```scala mdoc
final case class Replica(primary: HostPort)

Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(
    ConfigProvider.fromMap(Map("primary" -> "db.internal:5432")).load(deriveConfig[Replica])
  ).getOrThrow()
}
```

On Scala 3, any given `Config[A]` in scope is also used as a `DeriveConfig[A]`, so `given Config[HostPort] = ...` works as well.

## Scala 2 and Scala 3 Differences

Derivation uses Magnolia on Scala 2 and Scala 3 mirrors on Scala 3. The results are the same except for these points:

| Behaviour                                         | Scala 2                         | Scala 3                                             |
| ------------------------------------------------- | ------------------------------- | --------------------------------------------------- |
| Value class (`extends AnyVal`)                    | Derived and unwrapped           | Needs an instance                                   |
| `@describe` on a case class                       | Added to each field's description | Ignored; `@describe` on a field works             |
| Annotations re-exported from `zio.config.magnolia` | All                            | `describe`, `name`, `discriminator`, `kebabCaseLegacy` only; import the others from `zio.config.derivation` |
| `derives Config` on a case class                  | Not available                   | Available                                           |
| `ConfigProvider#autoLoad[A]`                      | Not available                   | Loads `A` with its derived description              |
| A given `Config[A]` used as `DeriveConfig[A]`     | No                              | Yes                                                 |
| Building an instance from a `Config[A]`           | `DeriveConfig(config)`          | `DeriveConfig.from(config)` or `DeriveConfig(config)` |

`@describe` on a sealed trait is ignored on both versions.

## Limits

- Recursive types are not supported.
- Scala 3 `enum` types are not covered by tests; use a `sealed trait`.
- On Scala 2, two subtypes with the same short name in one sealed trait are read as `Name_0` and `Name_1`; Scala 3 does not rename them. Rename one with `@name`.

## Next Steps

[Derivation](./automatic-derivation-of-config.md) shows derivation step by step, with the annotations. The functions every derived description supports are listed in [Config descriptors](./config-descriptors.md).
