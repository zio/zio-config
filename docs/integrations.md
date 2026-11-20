---
id: integrations
title: "Integrations"
sidebar_label: "Integrations"
---

ZIO Config works with several libraries you may already use. Each integration is a separate module, so you add only the ones you need:

| Library                                              | Module                  | What it adds                                                                    |
| ---------------------------------------------------- | ----------------------- | ------------------------------------------------------------------------------- |
| [enumeratum](https://github.com/lloydmeta/enumeratum) | `zio-config-enumeratum` | Descriptions that read enumeratum enums by name or value                        |
| [cats](https://typelevel.org/cats/)                  | `zio-config-cats`       | Descriptions for cats data types, and type class instances for `Config`         |
| [scalaz](https://github.com/scalaz/scalaz)           | `zio-config-scalaz`     | Descriptions for scalaz data types, and an `Applicative[Config]`                |
| [pureconfig](https://pureconfig.github.io)           | `zio-config-pureconfig` | Reuse a pureconfig `ConfigReader` for one value                                 |
| [refined](https://github.com/fthomas/refined)        | `zio-config-refined`    | Refined-type validation. See [Validation](./automatic-validations.md).         |
| AWS                                                  | `zio-config-aws`, `zio-config-zio-aws` | Read AWS Parameter Store. See [AWS Parameter Store](./aws-parameter-store.md). |

Add the modules you need, with the same version as `zio-config`:

```scala
// Enumeratum integration (JVM)
libraryDependencies += "dev.zio" %% "zio-config-enumeratum" % "@VERSION@"

// Cats integration (JVM)
libraryDependencies += "dev.zio" %% "zio-config-cats"       % "@VERSION@"

// Scalaz integration (JVM)
libraryDependencies += "dev.zio" %% "zio-config-scalaz"     % "@VERSION@"

// PureConfig integration (JVM)
libraryDependencies += "dev.zio" %% "zio-config-pureconfig" % "@VERSION@"
```

`import zio.config._` brings the packages `zio.config.cats`, `zio.config.scalaz`, `zio.config.enumeratum` and `zio.config.pureconfig` into scope, and they hide the libraries' own top-level packages of the same name. After that import, write the library imports with `_root_`, as in `import _root_.cats.data.NonEmptyList`, or put them in a file that does not import `zio.config._`.

The examples on this page share these imports and a helper, `show`, which loads a description from a map:

```scala mdoc:silent:reset-object
import zio._
import zio.config._

def show[A](config: Config[A], entries: (String, String)*): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromMap(entries.toMap).load(config).either).getOrThrow()
  }
```

## enumeratum

enumeratum defines enums as sealed traits with a companion `Enum`. `zio.config.enumeratum.enum` reads one of its entries by name, without derivation:

```scala mdoc:silent
import _root_.enumeratum.{Enum, EnumEntry}
import zio.config.enumeratum._

sealed trait Color extends EnumEntry

object Color extends Enum[Color] {
  val values = findValues

  case object Red   extends Color
  case object Green extends Color
}

val color: Config[Color] = `enum`(Color).nested("color")
```

The name must match an entry; anything else fails with enumeratum's message:

```scala mdoc
show(color, "color" -> "Green")
show(color, "color" -> "Blue")
```

The module has one function per kind of enumeratum enum:

| Function                   | Reads                                     |
| -------------------------- | ----------------------------------------- |
| `` `enum`(e) ``            | An `Enum[A]` entry by its name            |
| `stringEnum(e)`            | A `StringEnum[A]` entry by its value      |
| `intEnum(e)`               | An `IntEnum[A]` entry by its value        |
| `longEnum(e)`              | A `LongEnum[A]` entry by its value        |
| `shortEnum(e)`             | A `ShortEnum[A]` entry by its value       |
| `byteEnum(e)`              | A `ByteEnum[A]` entry by its value        |
| `charEnum(e)`              | A `CharEnum[A]` entry by its value        |
| `nonEmptyString`           | A `String`, failing when it is empty      |

`enum` is a keyword in Scala 3, so write it with backticks.

## cats

`zio-config-cats` reads cats data types. Each function takes a description for the element type, with or without the name of the key that holds the collection:

| Function            | Result type                     | Fails when             |
| ------------------- | ------------------------------- | ---------------------- |
| `chain`             | `Chain[A]`                      | Never; may be empty    |
| `nonEmptyChain`     | `NonEmptyChain[A]`              | The list is empty      |
| `nonEmptyList`      | `NonEmptyList[A]`               | The list is empty      |
| `nonEmptyMap`       | `NonEmptyMap[String, A]`        | The table is empty     |

To read a non-empty list of servers from indexed keys:

```scala mdoc:silent
import _root_.cats.data.NonEmptyList
import zio.config.cats._

val servers: Config[NonEmptyList[String]] = nonEmptyList("servers")(Config.string)
```

At least one element is required:

```scala mdoc
show(servers, "servers[0]" -> "a.internal", "servers[1]" -> "b.internal")
```

`zio.config.cats.instances._` adds type class instances:

| Instance                    | Gives you                                                            |
| --------------------------- | -------------------------------------------------------------------- |
| `Functor[Config]`           | `map` through cats syntax and `Functor` APIs                         |
| `SemigroupK[Config]`        | `<+>`, which is `Config#orElse`                                      |
| `Semigroup[ConfigProvider]` | `\|+\|`, which is `ConfigProvider#orElse`                            |
| `Eq[Config.Error]`          | `===` on errors                                                      |

To fall back to a second key with `<+>`:

```scala mdoc:silent
import _root_.cats.syntax.all._
import zio.config.cats.instances._

val url: Config[String] = Config.string("DATABASE_URL") <+> Config.string("DB_URL")
```

When only the second key is set, its value is used:

```scala mdoc
show(url, "DB_URL" -> "postgres://localhost/app")
```

:::note
The instance behind `Functor[Config]` is an `Applicative`, but it is declared with the type `Functor`, so cats can't find an `Applicative` or `Semigroupal` for `Config`, and syntax such as `mapN` or `tupled` doesn't compile. Combine descriptions with `Config#zip` instead.
:::

## scalaz

`zio-config-scalaz` reads scalaz data types:

| Function       | Takes                                    | Result type     |
| -------------- | ---------------------------------------- | --------------- |
| `maybe`        | A `Config[A]`                            | `Maybe[A]`, `Empty` when the key is missing |
| `iList`        | A `Config[A]` for one element            | `IList[A]`      |
| `iSet`         | A `Config[A]` for one element, with an `Order[A]` | `ISet[A]` |
| `mapz`         | A `Config[Map[A, B]]`, with an `Order[A]` | `A ==>> B`     |

`iList` and `iSet` read a list like `Config.listOf` without a key, so add the key with `Config#nested`. `mapz` converts an existing map description, such as one from `Config.table`:

```scala mdoc:silent
import _root_.scalaz.{IList, Maybe}
import zio.config.scalaz._

val tags: Config[IList[String]] = iList(Config.string).nested("tags")
```

`maybe(Config.int("AGE"))` gives `Empty` when `AGE` is missing, and the tags are read from indexed keys:

```scala mdoc
show(maybe(Config.int("AGE")): Config[Maybe[Int]])
show(tags, "tags[0]" -> "blue", "tags[1]" -> "green")
```

`zio.config.scalaz.instances._` adds an `Applicative[Config]`, so the scalaz `Apply` functions combine descriptions:

```scala mdoc:silent
import _root_.scalaz.Applicative
import zio.config.scalaz.instances._

val total: Config[Int] = Applicative[Config].apply2(Config.int("A"), Config.int("B"))(_ + _)
```

Both keys are read and combined:

```scala mdoc
show(total, "A" -> "1", "B" -> "2")
```

## pureconfig

`fromPureconfig[A]` turns a pureconfig `ConfigReader[A]` into a `Config[A]`. It reads **one string value** at the current path, parses it as HOCON, and gives the result to the reader. It does not read a group of keys from the provider. Use it to reuse readers for single values while you move to ZIO Config:

```scala mdoc:silent
import _root_.pureconfig.ConfigReader
import zio.config.pureconfig._

final case class Port(value: Int)

object Port {
  implicit val reader: ConfigReader[Port] = ConfigReader[Int].map(Port(_))
}

val port: Config[Port] = fromPureconfig[Port].nested("port")
```

The value `8080` is parsed and passed to the reader:

```scala mdoc
show(port, "port" -> "8080")
```

A reader for a case class works only when the whole object is one string value, for example an environment variable that holds a HOCON object:

```scala mdoc:silent
final case class Endpoint(host: String, port: Int)

object Endpoint {
  implicit val reader: ConfigReader[Endpoint] = ConfigReader.forProduct2("host", "port")(Endpoint(_, _))
}
```

The value of `ENDPOINT` is a HOCON object in one string:

```scala mdoc
show(fromPureconfig[Endpoint].nested("ENDPOINT"), "ENDPOINT" -> "{ host = localhost, port = 8080 }")
```

To move a whole project from pureconfig, see [How to migrate from pureconfig](./how-to-migrate-from-pureconfig.md).

## Next Steps

[Validation](./automatic-validations.md) covers the refined integration, and [AWS Parameter Store](./aws-parameter-store.md) covers the AWS integration.
