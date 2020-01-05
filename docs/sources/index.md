---
id: sources_index
title:  "Sources"
---

Once you have the description, you can use it to read from various sources

## Adding dependency

If you are using sbt:

```scala

libraryDependencies += "dev.zio" %% "zio-config" % <version>

```

##### Optional Dependency with magnolia module

```scala

libraryDependencies += "dev.zio" %% "zio-config-magnolia" % <version>

```

##### Optional Dependency with refined module

```scala

libraryDependencies += "dev.zio" %% "zio-config-refined" % <version>

```


##### Optional Dependency with typesafe module

```scala

libraryDependencies += "dev.zio" %% "zio-config-typesafe" % <version>

```

## Describe the config by hand

We must be fetching the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`


```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._

```

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

## Fully automated Config Description

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia`.

```scala mdoc:silent

import zio.config.magnolia.ConfigDescriptorProvider._

val myConfigAutomatic = description[MyConfig]

```

`myConfig` and `myConfigAutomatic` are same description, and is of the same type. 

More examples on automatic derivation is in examples module of [zio-config](https://github.com/zio/zio-config)

## Define the source

```scala mdoc:silent
val map = 
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

  val result: IO[ReadErrorsVector[String, String], zio.config.Config[MyConfig]] = 
    Config.fromMap(map, myConfig)

```

Another way of doing this is:

```scala mdoc:silent
val source = ConfigSource.fromMap(map)

read(myConfig from source)
// IO[ReadErrorsVector[String, String], MyConfig]
```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application. 

## And more..
As of now, we succesffully read the config.
However zio-config is much more powerful. It can read, write, document and even report your configuration.
Keep reading .. 

