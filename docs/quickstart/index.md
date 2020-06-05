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
import zio.config.config
import zio.config.ConfigDescriptor, ConfigDescriptor._
import zio.config.ConfigSource, ConfigSource._
import zio.config.Config

```

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```
To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent

val myConfig: ConfigDescriptor[MyConfig] =
  (string("LDAP") |@| int("PORT")|@| string("DB_URL"))(MyConfig.apply, MyConfig.unapply)

 // ConfigDescriptor[ MyConfig]

```

To get a tuple,

```scala mdoc:silent

val myConfigTupled: ConfigDescriptor[(String, Int, String)] =
  (string("LDAP") |@| int("PORT")|@| string("DB_URL")).tupled


```

## Fully automated Config Description

If you don't like describing your configuration manually, and rely on the names of the parameter in the case class (or sealed trait),
there is a separate module called `zio-config-magnolia`.

```scala mdoc:silent

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

val myConfigAutomatic: ConfigDescriptor[MyConfig] = descriptor[MyConfig]

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
generateReport(myConfig, MyConfig("xyz", 8888, "postgres"))
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

### Separation of concerns with `narrow`

In bigger apps you can have a lot of components and, consequently - a lot of configuration fields. It's not ideal to pass around the whole configuration object as a dependency for all of those components: this way you break the separation of concerns principle. Component should be aware only about dependencies it cares about and uses somehow.

So to avoid that and do it in an ergonomic way, there's a `narrow` syntax extension for `ZLayer`, available under `import zio.config.syntax._` import:

```scala mdoc:silent
import zio._
import zio.config.typesafe._
import zio.config.syntax._
import zio.config.magnolia.DeriveConfigDescriptor

trait Endpoint
trait Repository

case class AppConfig(api: ApiConfig, db: DbConfig)
case class DbConfig (url: String,    driver: String)
case class ApiConfig(host: String,   port: Int)

val configDescription = DeriveConfigDescriptor.descriptor[AppConfig]

// components have only required dependencies
val endpoint: ZLayer[Has[ApiConfig], Nothing, Has[Endpoint]]    = ZLayer.fromService(_ => new Endpoint {})
val repository: ZLayer[Has[DbConfig], Nothing, Has[Repository]] = ZLayer.fromService(_ => new Repository {}) 

val cfg = TypesafeConfig.fromDefaultLoader(configDescription)

cfg.narrow(_.api) >>> endpoint // narrowing down to a proper config subtree
cfg.narrow(_.db) >>> repository
```
