---
id: read_index
title:  "Read"
---
You need this import everywhere

```
import zio.config._, ConfigDescriptor._

```

We must be fetching the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala

final case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
So let' define that as the first step.

The most minimalistic configuration description will 
have the required parameters in the environment and specifying its type.

```scala
val myConfig =
  ((string("LDAP") |@| int("PORT")|@| string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

```

To read a config, means it has to perform some effects, and for that reason, it returns with a ZIO.
To be specific an IO (type IO[E, A] = ZIO[Any, E, A])

```scala

val result: IO[ConfigErrors[String, String], MyConfig] = 
  Config.fromEnv(myConfig) // That's system environment

```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application. 

# Alternative way of running the config

```
val result = Config.fromEnv(myConfig)

```
This is the most preferred way. However, you could explicitly call `from` which is another combinator in `ConfigDescriptor`
and pass a specific config source.

```scala
val source = ConfigSource.fromEnv
val result = read(myConfig from source)

```

# Custom types
We love `Port` instead of `Int` that represents a db port.

In this scenario, you could do 

```scala

int("PORT").xmap(Port)(_.value)

```

where port is;

```scala
 final case class Port(value: Int) extends AnyVal

```

That is, 

```scala

 final case class MyConfig(ldap: String, port: Port, dburl: String)

 // Before
  val myConfig =
    ((string("LDAP") |@| int("PORT").xmap(Port)(_.value) |@|
      string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

```

# Optional Types

Say, dburl is an optional type, then it is as simple as `string("DB_URL").optional`.

`optional` is just a similar combinator as that of `xmap`.

```scala
final case class MyConfig(ldap: String, port: Port, dburl: Option[String])

val myConfig =
  ((string("LDAP") |@| int("PORT").xmap(Port)(_.value) |@| 
    string("DB_URL").optional))(MyConfig.apply, MyConfig.unapply)
```

# Default
Sometimes, we don't need an optional value and instead happy providing a default value.

Use `.default`.

```scala
final case class MyConfig(username: String, port: Int)

val myConfig =
  ((string("USERNAME").default("root-oh") |@| int("PORT")))(MyConfig.apply, MyConfig.unapply)

```

You can also do things like fully overriding the entire configuration, might be helpful for tests.

```scala
myConfig.default(MyConfig("test", 80))
```

# Either Types (orElseEither)

For instance, if you are ok accepting a token or username, then your target type is typically an Either.
In this case, you can use `orElseEither` or `<+>`.

The naming standards are propogated from the zio core library.

```scala
final case class MyConfig(usernameOrToken: Either[String, String], port: Int)

val myConfig =
  ((string("USERNAME").orElseEither(string("TOKEN")) |@| int("PORT")))(MyConfig.apply, MyConfig.unapply)

```

You can also use `<+>` combinator.

```scala
 string("USERNAME") <+> (string("TOKEN"))
```

You can apply the `Either` logic at a much more global level, as in, give me either a `Prod` or `Test` config.

```scala
final case class Dev(userName: String, password: String)
final case class Prod(token: String, code: Int)

type Config = Either[Prod, Dev]

val dev = (string("USERNAME") |@| string("PASSWORD"))(Dev.apply, Dev.unapply)
val prod = (string("TOKEN") |@| int("CODE"))(Prod.apply, Prod.unapply)

val config = prod <+> dev // that represents a description returning Config
// ConfigDescriptor[String, String, Config]

```

# OrElse

Sometimes, we can try two different values and pick one. That means, the target is "NOT" `Either` but any raw type.
In this scenario, you can use `orElse` or `<>`

Example:

```scala
final case class Prod(token: String, code: Int)

val config = 
  (string("TOKEN") orElse string("TOKEN_INFO") |@| int("CODE")) (Prod.apply, Prod.unapply)

```

It tries to fetch the value corresponding to "TOKEN", and if it fails, it tries "TOKEN_INFO" and returns the corresponding value.

You can also use `<>` combinator.

```scala
string("TOKEN") <> string("token") <> string("TOKEN_INFO") 
```

## Built-in Primitive Types

We have already seen `string("TOKEN")` and `int("PORT")` to fetch string and int types respectively.
We support the following:
```scala
string
boolean
byte
short
int
long
bigInt
float
double
bigDecimal
uri

```

This much will be least required knowledge to get you going about "read"ing the config. 
However, if you have complicated configurations to be handled,
rest of the documentation will be helpful

## Nesting 

In addition to the primitive types, zio-config provides a combinator for nesting a configuration within another.

```scala
  def nested[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A]
```

## Composing multiple configurations

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
