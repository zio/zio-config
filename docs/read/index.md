---
id: read_index
title:  "Read"
---

# Defining Configuration Descriptions

To be able to configure a Scala type `T`, it needs an associated `ConfigDescriptor[T]`.
The user defines that `ConfigDescriptor[T]` in terms of `ConfigDescriptor`s for more fundamental types.
zio-config provides the most fundamental types of all, in the form of `ConfigDescriptor` for primitives.
The user builds up their `ConfigDescriptor[T]` from there. 

## Built-in Primitive Types

zio-config provides these primitive `ConfigDescriptor`s.

```scala
  def string(path: String): ConfigDescriptor[String]
  def boolean(path: String): ConfigDescriptor[Boolean]
  def byte(path: String): ConfigDescriptor[Byte]
  def short(path: String): ConfigDescriptor[Short]
  def int(path: String): ConfigDescriptor[Int]
  def long(path: String): ConfigDescriptor[Long]
  def bigInt(path: String): ConfigDescriptor[BigInt]
  def float(path: String): ConfigDescriptor[Float]
  def double(path: String): ConfigDescriptor[Double]
  def bigDecimal(path: String): ConfigDescriptor[BigDecimal]
  def uri(path: String): ConfigDescriptor[URI]
```

## Nesting `ConfigDescriptor`s

In addition to the primitive types, zio-config provides a combinator for nesting a configuration within another.

```scala
  def nested[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A]
```

## Composing `ConfigDescriptor`s

To combine `ConfigDescriptor`s into a `ConfigDescriptor` for a higher-level type, use the `|@|` combinator.
For example:
 
```scala
  final case class Database(url: String, port: Int)

  val database: ConfigDescriptor[Database] =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)
```

## Documentation

As part of the `ConfigDescriptor` DSL, the user can specify documentation strings. These are ultimately used
to generate manual page and configuration report output.

To add documentation to a `ConfigDescriptor`, use the `?` syntax:

```scala
  nested("south") { database } ? "South details" |@| ...
```

The top level `ConfigDescriptor` can also be annotated with a documentation string.

```scala
  ( ... |@| ... )(AwsConfig, AwsConfig.unapply)) ? "The AWS details to be used"
```

## Optional

For optional configured values, use the `optional` combinator, of the form:

```scala
  final case class DataItem(oid: Option[Id], ...)

  val cId: ConfigDescriptor[Id] = string("kId").xmap(Id)(_.value)
  val cDataItem: ConfigDescriptor[DataItem] = (cId.optional |@| ...)(DataItem.apply, DataItem.unapply)
```

## Default

zio-config supports default values that are used in the event of a missing configuration value:

```scala
  (string("key").default("default-key")
```

## Coproduct

A coproduct is represented by Scala `Either`.

For example, for a `ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]]`:

```scala
  val cfg: ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]] =
    enterprise.orElseEither(password)
```

## Sequence

It is common to handle lists of configured values – ie a configured `List`. 
For this case, `ConfigDescriptor` provides the `sequence` method, which

```scala
def sequence[A](configList: List[ConfigDescriptor[A]]): ConfigDescriptor[List[A]]
``` 

First, dynamically form a `List` of `ConfigDescriptor`s for the target data type, and then invoke `sequence`
to group the list as a single `ConfigDescriptor`.

NOTE: `collectAll` is a synonym for `sequence`.

# Example: Reading Configuration

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database: ConfigDescriptor[Database] =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig: ConfigDescriptor[AwsConfig] =
    ((nested("south") { database } ? "South details" |@|
      nested("east") { database } ? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)) ? "asdf"

  // For simplicity in example, we use map source. Works with hoccon.
  val source: ConfigSource[String, String] =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )

  val runtime = new DefaultRuntime {}

  private val cfg: IO[ReadErrors[String, String], AwsConfig] = read(appConfig).provide(source)
```

yields the result:

```scala
  AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp")
```
