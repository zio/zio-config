---
id: how-to-migrate-from-pureconfig
title: "How to Migrate from pureconfig"
sidebar_label: "Migrate from pureconfig"
---

pureconfig reads a `ConfigSource` into a case class with `ConfigReader` instances. ZIO Config does the same job with `Config` descriptions, which it can derive. This guide moves an application from pureconfig to ZIO Config while it keeps reading the same `application.conf`.

## Goal

Replace pureconfig with ZIO Config, keeping the same HOCON files, keys and case classes.

## Step 1: Add the Modules

Add the core module, the HOCON provider and derivation:

```scala
// ZIO Config core: config descriptors and readers (JVM)
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"

// HOCON and JSON sources, built on Typesafe Config (JVM)
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"

// Automatic derivation of config descriptors (JVM)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

Both libraries read HOCON through Typesafe Config, so they can run side by side while you migrate.

## Step 2: Derive the Descriptions With pureconfig's Key Names

pureconfig's default naming turns a field called `poolSize` into the key `pool-size`. ZIO Config uses the field name as is, so call `Config#toKebabCase` on the derived description to read the same keys. It converts every key, including the keys of nested case classes. This is the pureconfig code being replaced:

**Before (pureconfig):**

```scala
import pureconfig._
import pureconfig.generic.auto._

final case class Database(url: String, poolSize: Int)
final case class AppConfig(serviceName: String, database: Database)

val config: AppConfig = ConfigSource.default.loadOrThrow[AppConfig]
```

The ZIO Config version derives the description and converts its keys:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

final case class Database(url: String, poolSize: Int)
final case class AppConfig(serviceName: String, database: Database)

val appConfig: Config[AppConfig] = deriveConfig[AppConfig].toKebabCase
```

It reads a file written for pureconfig without changes:

```scala mdoc
val pureconfigStyle: String =
  """
  service-name = "orders"
  database {
    url = "postgres://localhost/orders"
    pool-size = 10
  }
  """

Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(ConfigProvider.fromHoconString(pureconfigStyle).load(appConfig)).getOrThrow()
}
```

If you changed the naming with a `ProductHint`, use the matching conversion (`Config#toSnakeCase`, `Config#mapKey`) or the annotations on the [Derivation](./automatic-derivation-of-config.md) page. Check names that contain digits, such as `version2Name`; the conversions can split them differently.

## Step 3: Keep Sealed Traits Readable

pureconfig reads a sealed trait from a `type` field whose value is the kebab-case name of the subtype, including for case objects: `{ type = sms, number = "123" }` and `{ type = disabled }`. In ZIO Config, `@discriminator("type")` reads the subtype from the same field, but it expects the exact Scala name. Add `@name` to each subtype to keep the pureconfig values:

```scala mdoc:silent
import zio.config.derivation.{discriminator, name}

@discriminator("type")
sealed trait Notifier

object Notifier {
  @name("email") final case class Email(address: String) extends Notifier
  @name("sms") final case class Sms(number: String)      extends Notifier
  @name("disabled") case object Disabled                 extends Notifier
}

final case class Alerts(notifier: Notifier)
```

Both a case class and a case object read as they did with pureconfig:

```scala mdoc
def loadAlerts(hocon: String): Alerts =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromHoconString(hocon).load(deriveConfig[Alerts])).getOrThrow()
  }

loadAlerts("""notifier { type = sms, number = "123" }""")
loadAlerts("""notifier { type = disabled }""")
```

## Step 4: Load Where the Application Starts

`ConfigSource.default` reads `application.conf` with Typesafe Config's defaults. `ConfigProvider.fromResourcePath()` does the same. Instead of `loadOrThrow`, load the description as a ZIO effect, for example in the `bootstrap` of a `ZIOAppDefault` and with `ZIO.config`:

```scala mdoc:compile-only
object Main extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.setConfigProvider(ConfigProvider.fromResourcePath())

  def run =
    ZIO.config(appConfig).flatMap(config => Console.printLine(s"Starting ${config.serviceName}"))
}
```

A failed load is a `Config.Error` instead of `ConfigReaderFailures`. `Config.Error#prettyPrint` renders every problem at once; see [Errors](./errors.md).

## Step 5: Move Custom Readers

For each custom `ConfigReader`, write a `DeriveConfig` instance from an existing one. A reader such as `ConfigReader[String].emap(...)` becomes `DeriveConfig[String].mapOrFail(...)`; see [Writing an Instance](./derive-config.md#writing-an-instance).

While you migrate, `zio-config-pureconfig` can reuse a reader with `fromPureconfig[A]`. It reads a single string value and passes it to the reader, so it suits readers for single values, not case classes read from a group of keys. See [Integrations](./integrations.md#pureconfig).

## Mapping Table

The main pureconfig concepts map to these ZIO Config ones:

| pureconfig                                   | ZIO Config                                                            |
| -------------------------------------------- | --------------------------------------------------------------------- |
| `ConfigSource.default`                       | `ConfigProvider.fromResourcePath()`                                   |
| `ConfigSource.string(s)`, `ConfigSource.file(f)` | `ConfigProvider.fromHoconString(s)`, `ConfigProvider.fromHoconFile(f)` |
| `loadOrThrow[A]`, `load[A]`                  | `provider.load(description)` or `ZIO.config(description)`             |
| `ConfigReader[A]`                            | `Config[A]`, `DeriveConfig[A]`                                        |
| `pureconfig.generic.auto._`                  | `deriveConfig[A]`                                                     |
| Default kebab-case keys                      | `Config#toKebabCase`, `@kebabCase`                                    |
| `ProductHint` renames                        | `@name`, `Config#mapKey`, `Config#toSnakeCase`                        |
| `CoproductHint` with a `type` field          | `@discriminator("type")`, with `@name` for kebab-case values          |
| `ConfigReaderFailures`                       | `Config.Error`                                                        |

## Check

Load the same file with both libraries in a test and compare the results, and remove the pureconfig dependency only after those tests pass.

## Related

See [Derivation](./automatic-derivation-of-config.md) for annotations and sealed traits, and [HOCON and JSON](./hocon-provider.md) for every HOCON constructor.
