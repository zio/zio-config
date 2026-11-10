---
id: automatic-derivation-of-config
title: "Derivation"
sidebar_label: "Derivation"
---

Derivation writes a config description for you. You add `zio-config-magnolia`, call `deriveConfig[A]`, and get a `Config[A]` built from the field names and types of your case class or sealed trait. You can then rename keys, add descriptions and change how sealed traits are read, with annotations or with functions on the description.

The result of `deriveConfig[A]` is the same kind of value you would build by hand with `Config.string(...)`, so every function in [Config descriptors](./config-descriptors.md) still works on it.

## A first example

```scala mdoc:silent
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig

final case class Database(url: String, poolSize: Int)

val databaseConfig: Config[Database] = deriveConfig[Database]
```

`databaseConfig` reads the keys `url` and `poolSize`. A field of a case class that is nested inside another case class reads its keys under the field name. The example below uses this with `Database` inside `AppConfig`.

```scala mdoc:silent
final case class AppConfig(name: String, database: Database)

val appConfig: Config[AppConfig] = deriveConfig[AppConfig]
```

This reads the keys `name`, `database.url` and `database.poolSize`.

Load a derived description the same way as any other:

```scala mdoc:compile-only
import zio.config.typesafe._

val loaded: IO[Config.Error, AppConfig] =
  ConfigProvider
    .fromHoconString(
      """
      name = "orders"
      database {
        url = "postgres://localhost/orders"
        poolSize = 10
      }
      """
    )
    .load(appConfig)
```

## Sealed traits

A sealed trait is read by the name of its case class. Each case object or case class is a key:

```scala mdoc:silent
sealed trait Greeting

object Greeting {
  case object Hello extends Greeting
  case object Bye   extends Greeting
  final case class Custom(text: String) extends Greeting
}

final case class Page(greeting: Greeting)

val pageConfig: Config[Page] = deriveConfig[Page]
```

With this description, `greeting = Hello` in HOCON reads `Page(Hello)`. The `Custom` case class has a field, so its value is a nested object:

```scala mdoc:compile-only
import zio.config.typesafe._

val custom: IO[Config.Error, Page] =
  ConfigProvider
    .fromHoconString(
      """
      greeting {
        Custom {
          text = "hi there"
        }
      }
      """
    )
    .load(pageConfig)
```

Use `@discriminator` to read the case class name from a field instead of a nested key. See [Changing the shape of a sealed trait](#changing-the-shape-of-a-sealed-trait).

## Annotations

Annotations change the keys and descriptions that derivation produces. Each one is applied to a case class, a sealed trait, or a field.

| Annotation | Import | Effect |
| --- | --- | --- |
| `@name("key")` | `zio.config.magnolia.name` | Use `key` instead of the Scala name |
| `@describe("text")` | `zio.config.magnolia.describe` | Add a description, shown by [documentation generation](./documentation-and-reporting.md) |
| `@discriminator("type")` | `zio.config.magnolia.discriminator` | Read the case class name from a key named `type` |
| `@kebabCaseLegacy` | `zio.config.magnolia.kebabCaseLegacy` | Use the older kebab-case rules, described below |
| `@kebabCase` | `zio.config.derivation.kebabCase` | Convert the keys to kebab-case |
| `@snakeCase` | `zio.config.derivation.snakeCase` | Convert the keys to snake_case |

### Renaming a key

```scala mdoc:silent
import zio.config.magnolia.name

final case class Service(
  @name("listen-port") port: Int,
  host: String
)

val serviceConfig: Config[Service] = deriveConfig[Service]
```

`serviceConfig` reads `listen-port` and `host`. The Scala name `port` is not used as a key.

### Adding descriptions

`@describe` on a case class describes the whole product. On a field, it describes that key:

```scala mdoc:silent
import zio.config.magnolia.describe

@describe("Settings for the HTTP server")
final case class HttpSettings(
  @describe("Port the server listens on") port: Int,
  host: String
)

val httpConfig: Config[HttpSettings] = deriveConfig[HttpSettings]
```

The descriptions appear in the output of `generateDocs`. The reading behavior does not change. Equivalent hand-written code is `(Config.int("port") ?? "Port the server listens on" zip Config.string("host")).to[HttpSettings] ?? "Settings for the HTTP server"`.

### Key case

Use `@kebabCase` or `@snakeCase` on a case class to change how its field names become keys. A field called `firstName` reads `first-name` with `@kebabCase`, and `first_name` with `@snakeCase`:

```scala mdoc:silent
import zio.config.derivation.{kebabCase, snakeCase}

@kebabCase
final case class KebabPerson(firstName: String, lastName: String)

@snakeCase
final case class SnakePerson(firstName: String, lastName: String)

val kebabPerson: Config[KebabPerson] = deriveConfig[KebabPerson]
val snakePerson: Config[SnakePerson] = deriveConfig[SnakePerson]
```

`@kebabCaseLegacy` gives the old kebab-case rules, which split a key only where a lowercase letter is followed by an uppercase one. Under these rules `myValue123` becomes `my-value123`, and `QAndA` becomes `qand-a`. Use it to keep key names that were written with the 4.0.6 rules. It was added in 4.0.7; see [What's New](./whats-new-since-4-0-6.md).

For any other key change, call `mapKey` on the derived description:

```scala mdoc:silent
val upperKeys: Config[AppConfig] = deriveConfig[AppConfig].mapKey(_.toUpperCase)
```

## Changing the shape of a sealed trait

By default, a sealed trait is read by the name of the case class. Two annotations change this.

`@discriminator(keyName)` reads the case class name from a key, and the case class fields sit next to it. The default key name is `type`:

```scala mdoc:silent
import zio.config.magnolia.discriminator

@discriminator("type")
sealed trait Notifier

object Notifier {
  final case class Email(address: String) extends Notifier
  final case class Sms(number: String)    extends Notifier
}

final case class NotifyConfig(notifier: Notifier)

val notifyConfig: Config[NotifyConfig] = deriveConfig[NotifyConfig]
```

With this description, the HOCON below reads `NotifyConfig(Sms("123"))`:

```scala mdoc:compile-only
import zio.config.typesafe._

val sms: IO[Config.Error, NotifyConfig] =
  ConfigProvider
    .fromHoconString(
      """
      notifier {
        type = Sms
        number = "123"
      }
      """
    )
    .load(notifyConfig)
```

`@name` on the sealed trait itself makes its name a key, so the source must include the trait name too. This is rarely needed.

## Custom types and implicit instances

Derivation needs an instance of `DeriveConfig` for every field type. Built-in types such as `String`, `Int` and `List` have one. For a type from another library, or a value class, write an instance yourself:

```scala mdoc:silent
import zio.config.magnolia._

final case class AwsRegion(value: String) extends AnyVal

object AwsRegion {
  implicit val deriveAwsRegion: DeriveConfig[AwsRegion] =
    DeriveConfig[String].map(AwsRegion(_))
}

final case class Deployment(region: AwsRegion, replicas: Int)

val deployment: Config[Deployment] = deriveConfig[Deployment]
```

Put the instance in the companion object of a type you own. For a type from another library, put it in the scope where you call `deriveConfig`.

## Scala 3

On Scala 3, derivation works the same way, and you can also use `derives` on a case class:

```scala
import zio.config.magnolia.*
import zio.Config

final case class MyConfig(a: String, b: Int) derives Config
```

The `derives Config` clause uses the same rules as `deriveConfig`. Derivation does not support Scala 3 `enum` values reliably yet. Use `sealed trait` with `case class` or `case object` instead. Recursive types are not supported by automatic derivation.

## Limits

- Every field needs a `DeriveConfig` instance, or derivation fails at compile time with a message naming the field and its type.
- Derivation does not generate a config for recursive types.
- Scala 3 `enum` is not supported with a guarantee.

## Next steps

- [Config descriptors](./config-descriptors.md) shows how to write descriptions by hand.
- [Validation](./automatic-validations.md) adds refined-type predicates to derived fields.
- [How to override settings per environment](./how-to-override-per-environment.md) combines derived descriptions with several providers.
