---
id: how-to-migrate-from-pureconfig
title: "How to Migrate from pureconfig"
sidebar_label: "Migrate from pureconfig"
---

pureconfig reads a `ConfigSource` into a case class using `ConfigReader` instances. zio-config does the same job with `Config` descriptions. This guide moves a project in two steps, so that each step can be tested on its own.

## Goal

Replace pureconfig with zio-config in a project, keeping the same keys and the same case classes.

## Step 1: Keep the readers, and wrap them

Add `zio-config-pureconfig` next to the existing pureconfig dependency. Then turn each existing reader into a zio-config description with `fromPureconfig`:

```scala
import pureconfig.ConfigReader
import zio.config._
import zio.config.pureconfig._

final case class Database(url: String, poolSize: Int)

object Database {
  implicit val reader: ConfigReader[Database] = ConfigReader.forProduct2("url", "poolSize")(Database(_, _))
}

val databaseFromPureconfig: Config[Database] = fromPureconfig[Database]
```

Load it with a zio-config provider the same way as any other description. The program now uses zio-config for reading, and pureconfig is needed only for the readers.

## Step 2: Replace the readers with derivation

When the wrapped readers are no longer needed, describe each case class with derivation. Remove the `ConfigReader` instances and the pureconfig dependency:

```scala
import zio.config.magnolia.deriveConfig

final case class Database(url: String, poolSize: Int)

val databaseConfig: Config[Database] = deriveConfig[Database]
```

Keys follow the field names, as with pureconfig's default. If your pureconfig reader renamed keys with a `ProductHint`, use the annotations in [Derivation](./automatic-derivation-of-config.md), or `mapKey`, to get the same keys.

## Mapping table

| pureconfig | zio-config |
| --- | --- |
| `ConfigSource.default.loadOrThrow[A]` | `ConfigProvider.fromHoconString(...).load(desc)` |
| `ConfigReader[A]` | `Config[A]` or `deriveConfig[A]` |
| `ProductHint` field renames | `@name`, `mapKey` |
| `CoproductHint` | `@discriminator`, see [Derivation](./automatic-derivation-of-config.md) |
| `Either[ConfigReaderFailures, A]` | `IO[Config.Error, A]` |

Errors change type. A failed load returns a `Config.Error`, not `ConfigReaderFailures`. See [Errors](./errors.md).

## Check

- Run the existing tests against the zio-config description. Both loaders should give the same value for the same input.
- Remove the pureconfig dependency only after the tests pass.

## Notes

- The examples on this guide are not compiled by the documentation build, because the pureconfig module is not a docs dependency yet.
- Refer to [Integrations](./integrations.md) for the other libraries that work with zio-config.
