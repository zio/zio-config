---
id: read_index
title:  "Read"
---
You need this import everywhere

```
import zio.config._, ConfigDescriptor._

```

## A Simple example

We must be fetching the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala

final case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala
val myConfig =
  ((string("LDAP") |@| int("PORT")|@| string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

```

Type of `myConfig` is `ConfigDescriptor[String, String, MyConfig]`. 
Type says, running it by passing a source will return `MyConfig`

## Running the description to ZIO

To read a config, means it has to perform some effects, and for that reason, it returns with a ZIO.
To be specific it returns an `IO` where `type IO[E, A] = ZIO[Any, E, A]`

```scala
val result: IO[ConfigErrors[String, String], MyConfig] = 
  Config.fromEnv(myConfig) // That's system environment
```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application. 

We will not be discussing about running with ZIO again, as it is just the same regardless of what the description is.
We will discuss only about how to describe your configuration for the rest of this page.

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

## Multiple sources

While it isn't quite natural an application fetching configuration from multiple sources,
it is possible to have multi source configuration parsing.

```scala
final case class MyConfig(ldap: String, port: Int, dburl: Option[String])

val source1 = ConfigSource.fromProperty
val source2 = ConfigSource.fromEnv

val myConfig =
  ((string("LDAP").from(source1.orElse(source2)) |@| int("PORT").from(source1)) |@|
    string("DB_URL").optional.from(source2)))(MyConfig.apply, MyConfig.unapply)

val run = read(myConfig)

// we can also separately add new config

val run = read(myConfig from ConfigSource.fromMap(...))

// In this case, `ConfigSource.fromMap` will also be tried along with the sources that are already given.

```

We can reset the sources for the config using

```scala

myConfig.resetSource // equivalent to myConfig.fromNothing

```

By that way, in tests we could remove the sources from each parameter and ask it to get it
from a constant map for all of it.

```scala

val testConfig =
  myConfig
    .resetSource
    .from(ConfigSource.fromMap("LDAP" -> "x", "DB_URL" -> "y",  "PORT" -> "1235"))

```

## Custom types
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

## Optional Types

Say, dburl is an optional type, then it is as simple as 

```scala
string("DB_URL").optional
```

That is, 

```scala
final case class MyConfig(ldap: String, port: Port, dburl: Option[String])

val myConfig =
  ((string("LDAP") |@| int("PORT").xmap(Port)(_.value) |@| 
    string("DB_URL").optional))(MyConfig.apply, MyConfig.unapply)
```

## Default
Sometimes, we don't need an optional value and instead happy providing a default value.

```scala
 string("USERNAME").default
 ```

 That is,

```scala
final case class MyConfig(username: String, port: Int)

val myConfig =
  ((string("USERNAME").default("root-oh") |@| int("PORT")))(MyConfig.apply, MyConfig.unapply)

```

We can also do things like fully overriding the entire configuration; might be helpful for tests.

```scala
myConfig.default(MyConfig("test", 80))
```

## Either Types (orElseEither)

For instance, if We are ok accepting either a token or username, then our target type should be `Either[String, String]`.
In this case, We can use `orElseEither` or `<+>`.

```scala
string("USERNAME").orElseEither(string("TOKEN")
```

That is,

```scala
final case class MyConfig(usernameOrToken: Either[String, String], port: Int)

val myConfig =
  ((string("USERNAME").orElseEither(string("TOKEN")) |@| int("PORT")))(MyConfig.apply, MyConfig.unapply)

```

We can also use `<+>` combinator.

```scala
 string("USERNAME") <+> (string("TOKEN"))
```

We can apply the `Either` logic at a much more global level, as in, give me either a `Prod` or `Test` config.

```scala
final case class Dev(userName: String, password: String)
final case class Prod(token: String, code: Int)

type Config = Either[Prod, Dev]

val dev = (string("USERNAME") |@| string("PASSWORD"))(Dev.apply, Dev.unapply)
val prod = (string("TOKEN") |@| int("CODE"))(Prod.apply, Prod.unapply)

val config = prod <+> dev // that represents a description returning Config
// ConfigDescriptor[String, String, Config]

```

## OrElse

Sometimes, we can try two different values and pick one. That means, the target is "NOT" `Either` but any raw type.
In this scenario, We can use `orElse` or `<>`

```scala

string("TOKEN") orElse string("token")

```

Example:

```scala
final case class Prod(token: String, code: Int)

val config = 
  (string("TOKEN") orElse string("token_x") |@| int("CODE")) (Prod.apply, Prod.unapply)

```

It tries to fetch the value corresponding to "TOKEN", and if it fails, it tries "token_x" and returns the corresponding value.

We can also use `<>` combinator.

```scala
string("TOKEN") <> string("token") <> string("TOKEN_INFO") 
```

## Composing multiple configurations

This is more of a real life scenario, where you can different micro configurations for readability and maintainability.
 
```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c3: String)


  val database: ConfigDescriptor[Database] =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val config = (database |@| string("c3"))(AwsConfig.apply, AwsConfig.unapply)

```

## Nesting 

In addition to the primitive types, zio-config provides a combinator for nesting a configuration within another.

```scala
  def nested[A](path: String)(desc: ConfigDescriptor[A]): ConfigDescriptor[A]
```

This might not feel intuitive at first, however, zio-config is desgined and patiently added this feature to easily adapt to
any other configuration parsing libraries that deals with files, hoccons that supports nested configurations.

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    (nested("south") { database } |@|
      nested("east") { database } |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

  // Let's say, a nested configuration as a flattened map is just "." delimited keys in a flat map.
  val source =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )
    
  Config.fromMap(appConfig)
```

Note that, you can write this back as well. This is discussed in write section

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

## Alternative way of running it to ZIO

As you have seen before, the most preferred way is,

```
val result = Config.fromEnv(myConfig)

```
However, you could explicitly  `from` which is another combinator in `ConfigDescriptor`
and pass a specific config source. We have seen this before for handling multiple sources.

```scala
val source = ConfigSource.fromEnv
val result = read(myConfig from source)

```