---
id: tutorial-model-config-with-adts
title: "Tutorial: Model a Real Configuration With Case Classes and Sealed Traits"
sidebar_label: "Model a Configuration With ADTs"
description: "Learn to describe nested, optional, repeated and alternative settings with case classes and sealed traits, and load them from HOCON."
keywords:
  - "Configuration"
  - "Algebraic Data Types"
  - "Derivation"
  - "deriveConfig"
---

Welcome! This tutorial is for Scala developers who have loaded a first, flat configuration with ZIO Config, for example by following [Getting Started](./getting-started.md). You don't need to know anything else about ZIO Config to follow along.

Real configurations are rarely flat. They group settings, leave some out, repeat others, and offer a choice between alternatives. In Scala, we model exactly these shapes with case classes and sealed traits, which together are called algebraic data types (ADTs). ZIO Config reads such a model directly from a file.

## Learning Objectives

By the end of this tutorial, you will understand:

- How the shape of your Scala types decides the shape of the configuration file
- How optional fields and default values make settings optional
- How lists and maps are read
- How a sealed trait lets the file choose between alternatives

We'll learn these concepts through:

1. A flat case class
2. Nested case classes
3. Optional settings and defaults
4. Lists and maps
5. Alternatives with a sealed trait
6. Alternatives selected by a type field

We recommend reading from top to bottom — each section builds on the previous one.

## Background

ZIO Config separates *what* a program needs from *where* the values come from. The *what* is a description of type `Config[A]`; the *where* is a `ConfigProvider`, such as a HOCON file. When the program's needs are expressed as Scala types, `deriveConfig[A]` writes the description for us: every field becomes a key, every nested case class becomes a nested object, and every sealed trait becomes a choice. Changing the type changes what the program expects from the file.

Throughout this tutorial, we build the configuration of a small notification service, one piece at a time.

## 1. A Flat Case Class

Let's start with the simplest model: one case class with two fields. We also write a helper, `load`, that reads a HOCON string with a description and returns the result, so we can see what each step reads:

```scala mdoc:silent:reset-object
import zio._
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._

def load[A](config: Config[A], hocon: String): Either[Config.Error, A] =
  Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(ConfigProvider.fromHoconString(hocon).load(config).either).getOrThrow()
  }

final case class Service(name: String, port: Int)
```

The code above sets up everything we need:

- The four imports bring in ZIO, the ZIO Config syntax, derivation and the HOCON provider.
- `load` builds a provider from a HOCON string, loads the description, and runs the effect so that we can print the result.
- `Service` is our first model: a name and a port.

Now we derive the description and load a matching document:

```scala mdoc
load(deriveConfig[Service], """
  name = "notifier"
  port = 8080
""")
```

`deriveConfig[Service]` reads one key per field, named like the field. Try removing `port` from the document: the result becomes a `Left` with a `MissingData` error for `port`.

## 2. Nested Case Classes

Settings that belong together go into their own case class. A field whose type is a case class reads its keys under the field's name:

```scala mdoc:silent
final case class Smtp(host: String, port: Int)
final case class Notifier(service: Service, smtp: Smtp)
```

In the code above, `Smtp` groups the mail server settings, and `Notifier` holds both groups. The HOCON document mirrors the types, with one object per case class:

```scala mdoc
load(deriveConfig[Notifier], """
  service { name = "notifier", port = 8080 }
  smtp { host = "mail.internal", port = 25 }
""")
```

Notice that both `Service` and `Smtp` have a field called `port`, and there is no clash: one is read as `service.port`, the other as `smtp.port`. The nesting of the types became the nesting of the keys.

## 3. Optional Settings and Defaults

Not every setting must be in the file. ZIO Config gives us two ways to say so, both in the type. A field of type `Option[A]` may be missing, and a field with a default value falls back to it:

```scala mdoc:silent
final case class Smtp2(host: String, port: Int = 25, username: Option[String])
```

The code above changes our mail settings in two ways: `port: Int = 25` gives the port a default value, and `username: Option[String]` makes the user name optional.

Now a document with only the host is enough:

```scala mdoc
load(deriveConfig[Smtp2], """host = "mail.internal"""")
```

The port fell back to `25`, and the user name is `None`. Defaults only cover *missing* keys: try `port = "twenty-five"`, and the load fails instead of quietly using the default.

## 4. Lists and Maps

Some settings repeat. A field of type `List[A]` reads a HOCON array, and a field of type `Map[String, A]` reads every key inside an object:

```scala mdoc:silent
final case class Routing(admins: List[String], quietHours: Map[String, Int])
```

The code above models two kinds of repetition: `admins` is a list of addresses that receive every alert, and `quietHours` maps a team name to the hour when its quiet time starts.

The document uses an array and an object:

```scala mdoc
load(deriveConfig[Routing], """
  admins = ["ops@example.com", "cto@example.com"]
  quietHours { payments = 22, search = 23 }
""")
```

The list elements can be case classes too; each array element is then an object. The keys of the map come from the file, not from our code, which is what makes a map different from a case class.

## 5. Alternatives With a Sealed Trait

Our service can send notifications in different ways, and each way needs different settings. A sealed trait expresses "one of these":

```scala mdoc:silent
sealed trait Channel

object Channel {
  final case class Email(to: String)               extends Channel
  final case class Slack(webhook: String)          extends Channel
  case object Disabled                             extends Channel
}

final case class Alerts(channel: Channel)
```

The code above defines three alternatives: `Email` and `Slack` each carry their own settings, and `Disabled` carries no settings at all.

By default, the file selects an alternative by its name. A case class alternative is an object named after the class, and a case object is just its name:

```scala mdoc
load(deriveConfig[Alerts], """channel { Slack { webhook = "https://hooks.example/abc" } }""")
load(deriveConfig[Alerts], "channel = Disabled")
```

Here is the "aha" moment: the file can only express a combination that our types allow. Try `channel = Pigeon`, or `channel { Email {} }` without the `to` setting: both fail to load, because no alternative matches.

## 6. Alternatives Selected by a Type Field

Many teams prefer a flatter style, where a field such as `type` names the alternative and its settings sit next to it. The `@discriminator` annotation on the sealed trait switches to that style:

```scala mdoc:silent
import zio.config.derivation.discriminator

@discriminator("type")
sealed trait Channel2

object Channel2 {
  final case class Email(to: String)      extends Channel2
  final case class Slack(webhook: String) extends Channel2
  case object Disabled                    extends Channel2
}

final case class Alerts2(channel: Channel2)
```

The only change from the previous section is `@discriminator("type")`. Now the `type` key selects the alternative:

```scala mdoc
load(deriveConfig[Alerts2], """
  channel {
    type = Email
    to = "ops@example.com"
  }
""")
```

## Putting It Together

This example combines all the concepts we've learned into the full configuration of the notification service: nested groups, an optional user name, a default port, a list, a map, and a channel chosen by a type field. It reuses `Service`, `Routing` and `Channel2` from the sections above:

```scala mdoc:silent
final case class Mail(host: String, port: Int = 25, username: Option[String])

final case class NotificationService(
  service: Service,
  mail: Mail,
  routing: Routing,
  channel: Channel2
)

val notificationService: Config[NotificationService] = deriveConfig[NotificationService]
```

One HOCON document describes the whole service:

```scala mdoc
load(notificationService, """
  service { name = "notifier", port = 8080 }
  mail { host = "mail.internal" }
  routing {
    admins = ["ops@example.com"]
    quietHours { payments = 22 }
  }
  channel { type = Slack, webhook = "https://hooks.example/abc" }
""")
```

## What You've Learned

In this tutorial, you learned:

- That the shape of your case classes and sealed traits decides the shape of the configuration file
- How `Option` fields and default values make a setting optional, and that defaults cover only missing keys
- How `List` and `Map` fields read arrays and objects
- How a sealed trait lets the file choose one alternative, either by name or with `@discriminator`

You now have a solid foundation for modelling configuration with types. The next step is to load such a model in a running ZIO application.

## Where to Go Next

- **Ready to use it in an application?** Continue with [Tutorial: Load Configuration in a ZIO Application](./tutorial-config-in-a-zio-app.md).
- **Need different key names?** [Derivation](./automatic-derivation-of-config.md) covers `@name`, `@kebabCase` and the other annotations.
- **Want the full list of supported field types?** See the [DeriveConfig reference](./derive-config.md).
