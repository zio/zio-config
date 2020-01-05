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

## Read from config from various sources

There are more information on various sources in [here](../sources/index.md).

Below given is a simple example.

```scala mdoc:silent
val map = 
  Map(
    "LDAP" -> "xyz",
    "PORT" -> "8888",
    "DB_URL" -> "postgres"
  )

  val result = 
    Config.fromMap(map, myConfig)

  // IO[ReadErrorsVector[String, String], zio.config.Config[MyConfig]]   

```

You can run this to [completion](https://zio.dev/docs/getting_started.html#main) as in any zio application. 

## How to use config descriptor

#### Readers from configdescriptor

As mentioned before, you can use config descriptor to read from various sources.

```scala mdoc:silent
val source = ConfigSource.fromMap(map)

val anotherResult = 
  read(myConfig from source)
// IO[ReadErrorsVector[String, String], MyConfig]
```

Note that, this is almost similar to `Config.fromMap(map, myConfig)` in the previous section.

More details in [here](../configdescriptor/index.md).

#### Documentations from configdescriptor

More details in [here](../configdescriptor/index.md).

#### Report Generation from configdescriptor

More details in [here](../configdescriptor/index.md).

#### Writers from configdescriptor
More details in [here](../configdescriptor/index.md).

## Config is your ZIO environment

Take a look at the below example that shows an entire mini app.
This will tell you how to consider configuration as just a part of `Environment` of your ZIO functions across your application.

```scala mdoc:silent

import zio.ZIO
import zio.console.Console.Live.console._

case class ApplicationConfig(bridgeIp: String, userName: String)

object ApplicationConfig {
  val configuration =
    ((string("bridgeIp")) |@| string("username"))(ApplicationConfig.apply, ApplicationConfig.unapply)
}

object SimpleExampleMain extends zio.App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val pgm =
      for {
        config <- Config.fromPropertyFile("file-location", ApplicationConfig.configuration)
        _      <- SimpleExample.finalExecution.provide(config)
      } yield ()

    pgm.foldM(
      throwable => putStr(throwable.getMessage()) *> ZIO.succeed(1),
      _ => putStrLn("hurray !! Application ran successfully..") *> ZIO.succeed(0)
    )
  }
}

object SimpleExample {
  val printConfigs: ZIO[Config[ApplicationConfig], Nothing, Unit] =
    for {
      appConfig <- config[ApplicationConfig]
      _         <- putStrLn(appConfig.bridgeIp)
      _         <- putStrLn(appConfig.userName)
    } yield ()

  val finalExecution: ZIO[Config[ApplicationConfig], Nothing, Unit] =
    for {
      _ <- printConfigs
      _ <- putStrLn(s"processing data......")
    } yield ()
}

```