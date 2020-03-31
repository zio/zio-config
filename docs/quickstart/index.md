---
id: quickstart_index
title:  "Quick Start"
---


The library aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application.
There are many examples in [here](https://github.com/zio/zio-config/tree/master/examples/src/main/scala/zio/config/examples) straight away as well.

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

We must fetch the configuration from the environment to a case class (product) in scala. Let it be `MyConfig`


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
  (string("LDAP") |@| int("PORT")|@| string("DB_URL"))(MyConfig.apply, MyConfig.unapply)

 // ConfigDescriptor[String, String, MyConfig]

```

Type of `myConfig` is `ConfigDescriptor[String, String, MyConfig]`.

## Fully automated Config Description

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia`.

```scala mdoc:silent

import zio.config.magnolia.ConfigDescriptorProvider._

val myConfigAutomatic = description[MyConfig]
// ConfigDescriptor[String, String, MyConfig]

```

`myConfig` and `myConfigAutomatic` are same description, and is of the same type. 

More examples on automatic derivation is in examples module of [zio-config](https://github.com/zio/zio-config)

## Read config from various sources

There are more information on various sources in [here](../sources/index.md).

Below given is a simple example.

```scala mdoc:silent
val map =
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

val source = ConfigSource.fromMap(map)

read(myConfig from source)
// Either[ReadError[String], MyConfig]

// Alternatively, you can rely on `Config.from..` pattern to get ZLayers.
val result =
  Config.fromMap(map, myConfig)

// Layer[ReadError[String], Config[A]]  

```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application.

## How to use config descriptor

### Readers from configdescriptor

As mentioned before, you can use config descriptor to read from various sources.

```scala mdoc:silent

val anotherResult =
  read(myConfig from source)
// Either[ReadError[String], MyConfig]
```

Note that, this is almost similar to `Config.fromMap(map, myConfig)` in the previous section.

More details in [here](../configdescriptor/index.md).

### Documentations using configdescriptor

```scala mdoc:silent
generateDocs(myConfig)
//Creates documentation (automatic)


val betterConfig =
  (string("LDAP") ?? "Related to auth" |@|  int("PORT") ?? "Database port" |@|
    string("DB_URL") ?? "url of database"
   )(MyConfig.apply, MyConfig.unapply)

generateDocs(betterConfig)
// Custom documentation along with auto generated docs
```

More details in [here](../configdescriptor/index.md).


### Writers from configdescriptor

```scala mdoc:silent

write(myConfig, MyConfig("xyz", 8888, "postgres")).map(_.flattenString())
//  Map("LDAP" -> "xyz", "PORT" -> "8888", "DB_URL" -> "postgres")

```

More details in [here](../configdescriptor/index.md).

### Report generation from configdescriptor


```scala mdoc:silent
generateDocsWithValue(myConfig, MyConfig("xyz", 8888, "postgres"))
// Generates documentation showing value of each parameter

```


More details in [here](../configdescriptor/index.md).

## Config is your ZIO environment

Take a look at the below example that shows an entire mini app.
This will tell you how to consider configuration as just a part of `Environment` of your ZIO functions across your application.

```scala mdoc:silent

import zio.{ ZIO, ZLayer }
import zio.console._

case class ApplicationConfig(bridgeIp: String, userName: String)

val configuration =
  (string("bridgeIp") |@| string("username"))(ApplicationConfig.apply, ApplicationConfig.unapply)

val finalExecution: ZIO[Config[ApplicationConfig] with Console, Nothing, Unit] =
  for {
    appConfig <- config[ApplicationConfig]
    _         <- putStrLn(appConfig.bridgeIp)
    _         <- putStrLn(appConfig.userName)
  } yield ()

val configLayer = Config.fromPropertiesFile("file-location", configuration)

// Main App
val pgm = finalExecution.provideLayer(configLayer ++ Console.live)

```
