---
id: configdescriptor_index
title:  "ConfigDescriptor"
---

Config Descriptor is the core of your configuration management. You can write a description by hand, 
or rely on zio-config-magnolia that can automatically generate the description for you, based on the case classes (pr sealed traits)
that represents your config.

```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._
```

## A Simple example

We must be fetching the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent
val myConfig =
  ((string("LDAP") |@| int("PORT")|@| string("DB_URL")))(MyConfig.apply, MyConfig.unapply)

```

Type of `myConfig` is `ConfigDescriptor[String, String, MyConfig]`. 
Type says, running it by passing a source will return `MyConfig`

## Fully Automated Config Description: zio-config-magnolia

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia` which uses `Magnolia` library to automatically derive the descriptions for you.

```scala mdoc:silent

import zio.config.magnolia.ConfigDescriptorProvider._

val myConfigAutomatic = description[MyConfig]

```

`myConfig` and `myConfigAutomatic` are same description, and is of the same type. 

If you need more control over the description,
probably you may choose to write it manually (such as, for adding extra documentations). 
More examples on automatic derivation is in examples module of [zio-config](https://github.com/zio/zio-config)

## Running the description to ZIO

To read a config, means it has to perform some effects, and for that reason, it returns with a ZIO.
To be specific it returns an `IO` where `type IO[E, A] = ZIO[Any, E, A]`

```scala mdoc:silent
val result: IO[ReadErrorsVector[String, String], zio.config.Config[MyConfig]] = 
  Config.fromEnv(myConfig) // That's system environment
```

Another way of doing this is:

```scala mdoc:silent
val source = ConfigSource.fromEnv(None)

read(myConfig from source)
// IO[ReadErrorsVector[String, String], MyConfig]
```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application. 

We will not be discussing about running with ZIO again, as it is just the same regardless of what the description is.
We will discuss only about how to describe your configuration for the rest of this page.

## Built-in types

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

## Optional Types

Say, dburl is an optional type, then it is as simple as 

```scala mdoc:silent
string("DB_URL").optional
```

That is, 

```scala mdoc:silent
case class MyConfigWithOptionalUrl(ldap: String, port: Port, dburl: Option[String])

val myConfigOptional =
  ((string("LDAP") |@| int("PORT").xmap(Port)(_.value) |@| 
    string("DB_URL").optional))(MyConfigWithOptionalUrl.apply, MyConfigWithOptionalUrl.unapply)

```

## Default
Sometimes, we don't need an optional value and instead happy providing a default value.

```scala mdoc:silent
 
 val defaultConfig = 
  string("USERNAME").default("ROOT")
 
```

 That is,

```scala mdoc:silent

case class MyConfigWithDefaultUserName(username: String, port: Int)

val myConfigDefault =
  ((string("USERNAME").default("root-oh") |@| int("PORT")))(MyConfigWithDefaultUserName.apply, MyConfigWithDefaultUserName.unapply)

```

We can also do things like fully overriding the entire configuration; might be helpful for tests.

```scala mdoc:silent
myConfigDefault.default(MyConfigWithDefaultUserName("test", 80))
```

## Custom types
We love `Port` instead of `Int` that represents a db port.

In this scenario, you could do 

```scala mdoc:silent

int("PORT").xmap(Port)(_.value)

```

where port is;

```scala mdoc:silent
 case class Port(value: Int)

```

That is, 

```scala mdoc:silent

 case class MyCustomConfig(ldap: String, port: Port, dburl: String)

 // Before
  val myConfigWithCustomType =
    ((string("LDAP") |@| int("PORT").xmap(Port)(_.value) |@|
      string("DB_URL")))(MyCustomConfig.apply, MyCustomConfig.unapply)

```

## Multiple sources

While it may not be always a good idea having to rely on multiple sources to form the application config,
zio-config do support this scenario. This can happen in complex applications.


```scala mdoc:silent
val source1 = ConfigSource.fromProperty(None)
val source2 = ConfigSource.fromEnv(None)

val myMultipleSourceConfig =
  ((string("LDAP").from(source1.orElse(source2)) |@| int("PORT").xmap(Port)(_.value).from(source1)) |@|
    string("DB_URL").optional.from(source2))(MyConfigWithOptionalUrl.apply, MyConfigWithOptionalUrl.unapply)

read(myMultipleSourceConfig)

// we can also separately add new config

read(myMultipleSourceConfig from ConfigSource.fromMap(Map.empty))

// In this case, `ConfigSource.fromMap` will also be tried along with the sources that are already given.

```

We can reset the sources for the config using  

```scala mdoc:silent

myMultipleSourceConfig.unsourced

```

By that way, in tests we could remove the sources from each parameter and ask it to get it
from a constant map for all of it.

```scala mdoc:silent

val testConfig =
  myMultipleSourceConfig
    .unsourced
    .from(ConfigSource.fromMap(Map("LDAP" -> "x", "DB_URL" -> "y",  "PORT" -> "1235")))

```

_NOTE_: As of now we support system env, system property and a constant map in `ConfigSource`.
We will be adding more, however, it will be easy to have any custom source. It is kept as simple as possible.

## Either Types (orElseEither)

For instance, if we are ok accepting either a token or username, then our target type should be `Either[String, String]`.
In this case, We can use `orElseEither` or `<+>`.

```scala mdoc:silent
string("USERNAME").orElseEither(string("TOKEN"))
```

That is,

```scala mdoc:silent
case class MyConfigWithEither(usernameOrToken: Either[String, String], port: Int)

val myConfigEither =
  ((string("USERNAME").orElseEither(string("TOKEN")) |@| int("PORT")))(MyConfigWithEither.apply, MyConfigWithEither.unapply)

```

We can also use `<+>` combinator.

```scala mdoc:silent
 string("USERNAME") <+> (string("TOKEN"))
```

We can apply the `Either` logic at a much more global level, as in, give me either a `Prod` or `Test` config.

```scala mdoc:silent
case class Dev(userName: String, password: String)
case class Prod(token: String, code: Int)

type Config = Either[Prod, Dev]

val dev = (string("USERNAME") |@| string("PASSWORD"))(Dev.apply, Dev.unapply)
val prod = (string("TOKEN") |@| int("CODE"))(Prod.apply, Prod.unapply)

prod <+> dev // that represents a description returning Config
// ConfigDescriptor[String, String, Config]

```

## OrElse

Sometimes, we can try two different values and pick one. That means, the target is "NOT" `Either` but any raw type.
In this scenario, We can use `orElse` or `<>`

```scala mdoc:silent

string("TOKEN") orElse string("token")

```

Example:

```scala mdoc:silent
val configOrElse = 
  (string("TOKEN").orElse(string("token_x")) |@| int("CODE")) (Prod.apply, Prod.unapply)

```

It tries to fetch the value corresponding to "TOKEN", and if it fails, it tries "token_x" and returns the corresponding value.

We can also use `<>` combinator.

```scala mdoc:silent
string("TOKEN") <> string("token") <> string("TOKEN_INFO") 
```

## Composing multiple configurations

This is more of a real life scenario, where you can different micro configurations for readability and maintainability.
 
```scala mdoc:silent
  case class Database(url: String, port: Int)
  case class AwsConfig(c1: Database, c3: String)


  val databaseConfig =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  (databaseConfig |@| string("c3"))(AwsConfig.apply, AwsConfig.unapply)

```

## Nesting 

In addition to the primitive types, zio-config provides a combinator for nesting a configuration within another.

This might not feel intuitive at first, however, zio-config is desgined and patiently added this feature to easily adapt to
any other configuration parsing libraries that deals with files, hoccons that supports nested configurations.

```scala mdoc:silent
  case class AwsConfigExtended(c1: Database, c2: Database, c3: String)

  val appConfig =
    (nested("south") { databaseConfig } |@|
      nested("east") { databaseConfig } |@|
      string("appName"))(AwsConfigExtended, AwsConfigExtended.unapply)

  // Let's say, a nested configuration as a flattened map is just "." delimited keys in a flat map.
  val constantMap =
    Map(
      "south.connection" -> "abc.com",
      "east.connection"  -> "xyz.com",
      "east.port"        -> "8888",
      "south.port"       -> "8111",
      "appName"          -> "myApp"
    )
    
  Config.fromMap(constantMap, appConfig)
```

Note that, you can write this back as well. This is discussed in write section

## CollectAll (Sequence)

It is common to handle lists of configured values – ie a configured `List`. 
For this case, we have `collectAll` method (also called `sequence`).

There are many ways you could get a list of config. 
Here we show a very naive version of it.

```scala mdoc:silent
 def database(i: Int) = 
   (string(s"${i}_URL") |@| int(s"${i}_PORT"))(Database, Database.unapply)

 val list: ConfigDescriptor[String, String, ::[Database]] =
   collectAll(::(database(0), (1 to 10).map(database).toList)) // collectAll takes `::` (cons, representing non-empty list) instead of a `List`.

```
Running this to ZIO will, obviously comes up with a List[Database]

NOTE: `collectAll` is a synonym for `sequence`.
