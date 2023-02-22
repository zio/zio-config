---
id: dive-into-zio-config
title: "Dive Into ZIO Config"
---

__Note that this documentation is for 1.x series. For newer versions, please refer to [docs](https://github.com/zio/zio-config/tree/master/docs) section in GitHub.__

## Describe the config by hand

We must fetch the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`

```scala mdoc:silent
import zio.IO

import zio.config._
import zio.ConfigProvider
import zio.Config, Config._

```

```scala mdoc:silent
case class MyConfig(ldap: String, port: Int, dburl: String)
```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent
val myConfig: Config[MyConfig] =
  (string("LDAP") zip int("PORT") zip string("DB_URL")).to[MyConfig]

 // Config[MyConfig]
```

To get a tuple,

```scala mdoc:silent
val myConfigTupled: Config[(String, Int, String)] =
  (string("LDAP") zip int("PORT") zip string("DB_URL"))
```

## Fully automated Config Description

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia`.

Note:  `zio-config-shapeless` is an alternative to `zio-config-magnolia` to support scala 2.11 projects.
It will be deprecated once we find users have moved on from scala 2.11.


```scala mdoc:silent
import zio.config._
import zio.config.magnolia._

val myConfigAutomatic = deriveConfig[MyConfig]
```

`myConfig` and `myConfigAutomatic` are same description, and is of the same type.

Refer to API docs for more explanations on [descriptor](https://javadoc.io/static/dev.zio/zio-config-magnolia_2.13/1.0.0-RC31-1/zio/config/magnolia/index.html#descriptor[A](implicitconfig:zio.config.magnolia.package.Descriptor[A]):zio.Config[A])
More examples on automatic derivation is in examples module of [zio-config](https://github.com/zio/zio-config)

## Read config from various sources

There are more information on various sources in [here](read-from-various-sources.md).

Below given is a simple example.

```scala mdoc:silent
val map =
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

val source = ConfigProvider.fromMap(map)

source.load(myConfig)

```

### Readers from configdescriptor

As mentioned before, you can use config descriptor to read from various sources.

```scala mdoc:silent
val anotherResult =
  source.load(myConfig)
```

Note that, this is almost similar to `Config.fromMap(map, myConfig)` in the previous section.

More details in [here](manual-creation-of-config-descriptor.md).

### Documentations using Config

```scala mdoc:silent
generateDocs(myConfig)
//Creates documentation (automatic)

val betterConfig =
  (string("LDAP") ?? "Related to auth" zip int("PORT") ?? "Database port" zip
    string("DB_URL") ?? "url of database"
   ).to[MyConfig]

generateDocs(betterConfig).toTable.toGithubFlavouredMarkdown
// Custom documentation along with auto generated docs
```

More details in [here](manual-creation-of-config-descriptor.md).

### Accumulating all errors

For any misconfiguration, the ReadError collects all of them with proper semantics: `AndErrors` and `OrErrors`.
Instead of directly printing misconfigurations, the `ReadError.prettyPrint` shows the path, detail of collected misconfigurations.

1. All misconfigurations of `AndErrors` are put in parallel lines.
```text
╥
╠══╗ 
║  ║ FormatError
║ MissingValue
``` 
2. `OrErrors` are in the same line which indicates a sequential misconfiguration

```text
╥
╠MissingValue
║
╠FormatError
```

Here is a complete example:

```text
   ReadError:
   ╥
   ╠══╦══╗
   ║  ║  ║
   ║  ║  ╠─MissingValue
   ║  ║  ║ path: var2
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ╠─MissingValue path: envvar3
   ║  ║  ║ path: var3
   ║  ║  ║ Details: value of type string
   ║  ║  ║ 
   ║  ║  ▼
   ║  ║
   ║  ╠─FormatError
   ║  ║ cause: Provided value is wrong, expecting the type int
   ║  ║ path: var1
   ║  ▼
   ▼
```

It says, fix `FormatError` related to path "var1" in the source. For the next error, either provide var2 or var3
to fix `MissingValue` error.

**Note**: Use prettyPrint method to avoid having to avoid seeing highly nested ReadErrors, that can be difficult to read.

### Example of mapping keys

Now on, the only way to change keys is as follows:

```scala
  // mapKey is just a function in `Config` that pre-existed

  val config = deriveConfig[Config].mapKey(_.toUpperCase)
```

## Inbuilt support for pure-config

Many users make use of the label `type` in HOCON files to annotate the type of the coproduct.
Now on, zio-config has inbuilt support for reading such a file/string using `descriptorForPureConfig`.


```scala
import zio.config._, typesafe._, magnolia._

@nameWithLabel("type")
sealed trait X
case class A(name: String) extends X
case class B(age: Int) extends X

case class AppConfig(x: X)

val str =
  s"""
   x : {
     type = A
     name = jon
   }
  """

ConfigProvider.fromHoconString(str).load(deriveConfig[AppConfig])

```