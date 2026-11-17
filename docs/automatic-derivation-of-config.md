---
id: automatic-derivation-of-config
title: "Derivation"
sidebar_label: "Derivation"
---

Derivation writes a config description for you. You add `zio-config-magnolia`, call `deriveConfig[A]`, and get a `Config[A]` built from the field names and types of your case class or sealed trait. Annotations rename keys, add descriptions and change how sealed traits are read.

The result of `deriveConfig[A]` is the same kind of value you would build by hand with `Config.string(...)`, so every function in [Config descriptors](./config-descriptors.md) still works on it. The type class behind derivation, its built-in instances and the differences between Scala 2 and Scala 3 are listed in the [DeriveConfig reference](./derive-config.md).

To use derivation, add the module next to the core one:

```scala
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
```

The examples on this page share these imports and a helper, `fromHocon`, which loads a description from a HOCON string:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

def fromHocon[A](config: Config[A], hocon: String): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromHoconString(hocon).load(config).either).getOrThrow()
  }
```

## Case Classes

Each field of a case class becomes a key with the field's name. A field whose type is another case class reads its keys under the field name:

```scala mdoc:silent
final case class Database(url: String, poolSize: Int)
final case class AppConfig(name: String, database: Database)

val appConfig: Config[AppConfig] = deriveConfig[AppConfig]
```

`appConfig` reads the keys `name`, `database.url` and `database.poolSize`:

```scala mdoc
fromHocon(appConfig, """
  name = "orders"
  database {
    url = "postgres://localhost/orders"
    poolSize = 10
  }
""")
```

### Optional Fields and Default Values

A field of type `Option[A]` is optional: a missing key gives `None`. A field with a default value in the case class uses that value when its key is missing:

```scala mdoc:silent
final case class Http(host: String, port: Int = 8080, proxy: Option[String])
```

Only `host` is set in the source below; `port` takes its default and `proxy` is `None`:

```scala mdoc
fromHocon(deriveConfig[Http], """host = "0.0.0.0"""")
```

As with `Config#withDefault`, the default applies only when the key is missing. A value that can't be parsed still fails.

### Collections and Maps

Fields of type `List`, `Seq`, `Vector`, `Set`, `Chunk` and `NonEmptyChunk` read a list. A field of type `Map[String, A]` reads every key under its name:

```scala mdoc:silent
final case class Routing(upstreams: List[String], weights: Map[String, Int])
```

In HOCON, the list is an array and the map is an object:

```scala mdoc
fromHocon(deriveConfig[Routing], """
  upstreams = ["a.internal", "b.internal"]
  weights { a = 3, b = 1 }
""")
```

## Sealed Traits

A sealed trait is read by the name of its subtype. A case object is the plain name. A case class with fields sits under a key with its name:

```scala mdoc:silent
sealed trait Greeting

object Greeting {
  case object Hello                     extends Greeting
  case object Bye                       extends Greeting
  final case class Custom(text: String) extends Greeting
}

final case class Page(greeting: Greeting)

val pageConfig: Config[Page] = deriveConfig[Page]
```

With this description, `greeting = Hello` reads the case object:

```scala mdoc
fromHocon(pageConfig, "greeting = Hello")
```

The `Custom` case class has a field, so its value is a nested object named after the class:

```scala mdoc
fromHocon(pageConfig, """greeting { Custom { text = "hi there" } }""")
```

### Reading the Subtype From a Field

`@discriminator(keyName)` on the sealed trait reads the subtype's name from a key, and the case class fields sit next to it. The default key name is `type`. The value is the exact name of the subtype, such as `Sms`:

```scala mdoc:silent
import zio.config.derivation.discriminator

@discriminator("type")
sealed trait Notifier

object Notifier {
  final case class Email(address: String) extends Notifier
  final case class Sms(number: String)    extends Notifier
  case object Disabled                    extends Notifier
}

final case class NotifyConfig(notifier: Notifier)

val notifyConfig: Config[NotifyConfig] = deriveConfig[NotifyConfig]
```

The `type` key selects `Sms`, and `number` is read next to it:

```scala mdoc
fromHocon(notifyConfig, """notifier { type = Sms, number = "123" }""")
```

A case object needs only the `type` key:

```scala mdoc
fromHocon(notifyConfig, "notifier { type = Disabled }")
```

## Annotations

Annotations change the keys and descriptions that derivation produces. Import them from `zio.config.derivation`, which works on Scala 2 and Scala 3. On Scala 2, `zio.config.magnolia` re-exports all of them; on Scala 3 it re-exports only `describe`, `name`, `discriminator` and `kebabCaseLegacy`.

| Annotation               | Put it on                      | Effect                                                                                     |
| ------------------------ | ------------------------------ | ------------------------------------------------------------------------------------------ |
| `@name("key")`           | A field, a subtype, a sealed trait | Use `key` instead of the Scala name                                                    |
| `@describe("text")`      | A field, a case class          | Add a description, shown by [documentation generation](./documentation-and-reporting.md) |
| `@discriminator("type")` | A sealed trait                 | Read the subtype's name from the key `type`                                                |
| `@kebabCase`             | A case class                   | Convert its field names to kebab-case                                                      |
| `@snakeCase`             | A case class                   | Convert its field names to snake_case                                                      |
| `@kebabCaseLegacy`       | A case class                   | Convert its field names with the older kebab-case rules                                    |
| `@prefix("p")`           | A case class                   | Add a prefix to its field names: `port` becomes `pPort`                                    |
| `@postfix("p")`          | A case class                   | Add a suffix to its field names: `port` becomes `portP`                                    |

### Renaming a Key

`@name` on a field replaces the key. Key-case annotations on the class don't change a field with `@name`:

```scala mdoc:silent
import zio.config.derivation.name

final case class Service(@name("listen-port") port: Int, host: String)
```

`Service` reads `listen-port` and `host`; the Scala name `port` is not used:

```scala mdoc
fromHocon(deriveConfig[Service], """
  listen-port = 9000
  host = "localhost"
""")
```

`@name` on a subtype of a sealed trait changes the name that selects it, and `@name` on the sealed trait itself adds one more level of nesting with that name.

### Adding Descriptions

`@describe` on a field describes that key. On a case class, it describes the whole product, and the text is added to the description of each field. Descriptions appear in the output of `generateDocs`; they don't change how keys are read:

```scala mdoc:silent
import zio.config.derivation.describe

@describe("Settings for the HTTP server")
final case class HttpSettings(@describe("Port the server listens on") port: Int, host: String)
```

The equivalent hand-written description uses `Config#??`:

```scala mdoc:silent
val handWritten: Config[HttpSettings] =
  ((Config.int("port") ?? "Port the server listens on") zip Config.string("host")).to[HttpSettings] ?? "Settings for the HTTP server"
```

On Scala 3, `@describe` on a case class is currently ignored; `@describe` on a field works on both versions.

### Key Case, Prefixes and Suffixes

`@kebabCase` and `@snakeCase` on a case class change how its own field names become keys. A field called `firstName` reads `first-name` with `@kebabCase`, and `first_name` with `@snakeCase`. They don't apply to the fields of nested case classes; annotate each class:

```scala mdoc:silent
import zio.config.derivation.{kebabCase, snakeCase}

@kebabCase
final case class KebabPerson(firstName: String, lastName: String)

@snakeCase
final case class SnakePerson(firstName: String, lastName: String)
```

The kebab-case class reads `first-name` and `last-name`:

```scala mdoc
fromHocon(deriveConfig[KebabPerson], """
  first-name = "Ada"
  last-name = "Lovelace"
""")
```

`@kebabCaseLegacy` gives the kebab-case rules from before ZIO Config 4.0.5. The two rules give the same keys for most names: `firstName`, `myValue123` and `instantIdQAndA` become `first-name`, `my-value123` and `instant-id-qand-a` under both. They differ only where a digit is followed by an uppercase letter: `version2Name` becomes `version2-name` with `@kebabCase` and `version2name` with `@kebabCaseLegacy`. Use the legacy rule to keep keys such as `version2name` that were written for the older rules. It is available since 4.0.7.

`@prefix` and `@postfix` on a case class add text before or after each field name. To keep camelCase, `@prefix` capitalizes the field name and `@postfix` capitalizes the added text. They are applied before the key-case annotation, so `@prefix("db") @kebabCase` turns `poolSize` into `db-pool-size`:

```scala mdoc:silent
import zio.config.derivation.prefix

@prefix("db") @kebabCase
final case class Pool(poolSize: Int, timeoutSeconds: Int)
```

The source uses the prefixed kebab-case keys:

```scala mdoc
fromHocon(deriveConfig[Pool], """
  db-pool-size = 10
  db-timeout-seconds = 5
""")
```

`@prefix` and `@postfix` are read only on the case class. On a single field they are ignored without a warning; use `@name` for one field.

For any other key change, call `Config#mapKey` on the derived description, for example `deriveConfig[AppConfig].mapKey(_.toUpperCase)`.

## Custom Types

Derivation needs a `DeriveConfig` instance for every field type. Built-in types such as `String`, `Int`, `Duration` and `List` have one. For any other type, put an instance in the type's companion object, built from an existing instance with `DeriveConfig#map` or `DeriveConfig#mapOrFail`. On Scala 2, a value class such as `final case class Port(value: Int) extends AnyVal` needs no instance; on Scala 3 it does.

The [DeriveConfig reference](./derive-config.md) lists the built-in instances and the methods for [writing an instance](./derive-config.md#writing-an-instance). [How to read a custom type](./how-to-read-a-custom-type.md) walks through a complete example.

## Scala 3

On Scala 3, `deriveConfig[A]` works the same way. A case class can also derive its description with `derives Config`, and ZIO Config adds `ConfigProvider#autoLoad`. This code is Scala 3 only:

```scala
import zio._
import zio.config.magnolia._

final case class MyConfig(host: String, port: Int = 8080) derives Config

val loaded: IO[Config.Error, MyConfig] =
  ConfigProvider.fromMap(Map("host" -> "localhost")).autoLoad[MyConfig]
```

`derives Config` puts a given `Config[MyConfig]` in the companion object, built with the same rules as `deriveConfig`. On Scala 3, any given `Config[A]` in scope also serves as the `DeriveConfig[A]` for fields of type `A`.

Scala 3 `enum` types are not covered by tests. Use a `sealed trait` with `case class` and `case object` subtypes.

## Limits

- Every field needs a `DeriveConfig` instance, or derivation fails at compile time.
- Recursive types are not supported. Write a recursive description by hand with `Config.defer`; see [Config descriptors](./config-descriptors.md).
- On Scala 2, two subtypes with the same short name in one sealed trait are read as `Name_0` and `Name_1`. Scala 3 does not rename them. Rename one with `@name`.

## Next Steps

- [DeriveConfig reference](./derive-config.md) lists the built-in instances and the differences between Scala 2 and Scala 3.
- [Validation](./automatic-validations.md) adds refined-type predicates to derived fields.
- [How to override settings per environment](./how-to-override-per-environment.md) combines derived descriptions with several providers.
