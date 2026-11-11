---
id: integrations
title: "Integrations"
sidebar_label: "Integrations"
---

zio-config works with several libraries you may already use. Each integration is a separate module, so you add only the ones you need. This page lists them and shows the most useful entry points.

| Library | Module | What it adds |
| --- | --- | --- |
| enumeratum | `zio-config-enumeratum` | Load enum values directly, without derivation |
| cats | `zio-config-cats` | `Config` instances, and cats data types such as `Chain` and `NonEmptyList` |
| scalaz | `zio-config-scalaz` | `Config` instances, and scalaz data types such as `IList` and `Maybe` |
| pureconfig | `zio-config-pureconfig` | Use an existing pureconfig `ConfigReader` as a zio-config description |
| refined | `zio-config-refined` | Refined-type validation. See [Validation](./automatic-validations.md). |
| AWS | `zio-config-aws`, `zio-config-zio-aws` | Read AWS Parameter Store. See [AWS Parameter Store](./aws-parameter-store.md). |

## enumeratum

[enumeratum](https://github.com/lloydmeta/enumeratum) defines enums as sealed traits with a companion `Enum`. `zio.config.enumeratum.enum` reads one of its values by name, without deriving a description:

```scala
import enumeratum.{Enum, EnumEntry}
import zio._
import zio.config._
import zio.config.enumeratum._
import zio.config.typesafe._

sealed trait Greeting extends EnumEntry

object Greeting extends Enum[Greeting] {
  val values = findValues

  case object Hello extends Greeting
  case object Bye   extends Greeting
}

val greeting: Config[Greeting] = `enum`(Greeting).nested("greeting")

val loaded: IO[Config.Error, Greeting] =
  ConfigProvider.fromMap(Map("greeting" -> "Hello")).load(greeting)
```

`loaded` succeeds with `Hello`. The module also has `intEnum`, `stringEnum`, `longEnum`, `shortEnum`, `byteEnum` and `charEnum` for the matching enumeratum types, and `nonEmptyString`.

## cats

With `zio-config-cats`, `Config` gets an `Applicative` instance. Use it to combine descriptions with `Applicative` syntax:

```scala
import cats.syntax.all._
import zio.config._
import zio.config.cats.instances._

val sum: Config[Int] =
  (Config.int("MARKS_1"), Config.int("MARKS_2")).mapN(_ + _)
```

The module also reads cats data types. `chain`, `nonEmptyChain`, `nonEmptyList` and `nonEmptyMap` each take a description for the element type:

```scala
import cats.data.NonEmptyList
import zio.config.cats._

val servers: Config[NonEmptyList[String]] =
  nonEmptyList("servers")(Config.string)
```

The prefix argument is the key that holds the list, as with `Config.listOf`.

## scalaz

`zio-config-scalaz` provides the same kind of instances for scalaz. It also reads scalaz types:

```scala
import scalaz.Maybe
import zio.config._
import zio.config.scalaz._

val age: Config[Maybe[Int]] = maybe(Config.int("AGE"))
```

`maybe` reads an optional value into `scalaz.Maybe`, and `iList`, `iSet` and `mapz` read `IList`, `ISet` and `==>>` values.

## pureconfig

If your application already uses [pureconfig](https://github.com/pureconfig/pureconfig), you can reuse its `ConfigReader` instances while you move to zio-config. `fromPureconfig` turns a reader into a zio-config description:

```scala
import pureconfig.ConfigReader
import zio.config.pureconfig._

final case class Port(value: Int)

object Port {
  implicit val reader: ConfigReader[Port] = ConfigReader[Int].map(Port(_))
}

val port = fromPureconfig[Port]
```

The step-by-step migration is in [How to migrate from pureconfig](./how-to-migrate-from-pureconfig.md).

## Modules to add

The examples above use these artifacts. Add the ones you need, with the same version as `zio-config`:

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-config-enumeratum" % "<version>",
  "dev.zio" %% "zio-config-cats"       % "<version>",
  "dev.zio" %% "zio-config-scalaz"     % "<version>",
  "dev.zio" %% "zio-config-pureconfig" % "<version>"
)
```

The examples on this page are not compiled by the documentation build. Their imports and signatures match the source, and a compile check is a follow-up item.
