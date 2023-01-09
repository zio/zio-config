---
id: index
title: "Getting Started with ZIO Config"
sidebar_label: "Getting Started"
---

[ZIO Config](https://zio.github.io/zio-config/) is a ZIO-based library for loading and parsing configuration sources.

_ZIO Config_ offloads all parsing and file formats to other libraries, and just focuses on being the _interface_ to configuration data throughout an application.

@PROJECT_BADGES@

## Introduction

In the real world, config retrieval is the first to develop applications. We mostly have some application config that should be loaded and parsed through our application. Doing such things manually is always boring and error-prone and also has lots of boilerplates.

The library aims to have a powerful & purely functional, yet a thin interface to access configuration information inside an application. 

The ZIO Config has a lot of features, and it is more than just a config parsing library. Let's enumerate some key features of this library:

- **Support for Various Sources** — It can read/write flat or nested configurations from/to various formats and sources.
- **Composable sources** — ZIO Config can compose sources of configuration, so we can have, e.g. environmental or command-line overrides.
- **Automatic Document Generation** — It can auto-generate documentation of configurations. So developers or DevOps engineers know how to configure the application.
- **Report generation** — It has a report generation that shows where each piece of configuration data came from.
- **Automatic Derivation** — It has built-in support for automatic derivation of readers and writers for case classes and sealed traits.
- **Type-level Constraints and Automatic Validation** — because it supports _Refined_ types, we can write type-level predicates which constrain the set of values described for data types.
- **Descriptive Errors** — It accumulates all errors and reports all of them to the user rather than failing fast.

Using a single definition of configuration requirements, which can be derived automatically from your data types, _ZIO Config_ offers a bundle of features for free:

* Read flat or nested config data from any format, with descriptive errors
* Write flat or nested config data into any format
* Compose sources of configuration, so you can have, e.g., environmental or command-line overrides
* Automatically generate documentation so devs / devops know how to configure the application
* Generate a report that shows where each piece of configuration data came from

If you are only interested in automatic derivation of configuration, find the details [here](http://zio.dev/zio-config/automatic-derivation-of-config-descriptor).

## Installation

In order to use this library, we need to add the following line in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config" % "@VERSION@" 
```

There are also some optional dependencies:

```scala
// Optional Dependency with magnolia module (Auto derivation)
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"

// Optional Dependency with refined module (Integration with refined library)
libraryDependencies += "dev.zio" %% "zio-config-refined" % "@VERSION@"

// Optional Dependency with typesafe module (HOCON/Json source)
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"

// Optional Dependency with yaml module (Yaml source)
libraryDependencies += "dev.zio" %% "zio-config-yaml" % "@VERSION@"

// Optional Dependency for a random generation of a config
libraryDependencies += "dev.zio" %% "zio-config-gen" % "@VERSION@"
```

## Example

Let's add these four lines to our `build.sbt` file as we are using these modules in our example:

```scala
libraryDependencies += "dev.zio" %% "zio-config"          % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-config-refined"  % "@VERSION@"
```

In this example we are reading from HOCON config format using type derivation:

```scala mdoc:compile-only
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.GreaterEqual
import zio._
import zio.config.magnolia.{describe, descriptor}
import zio.config.typesafe.TypesafeConfigSource

sealed trait DataSource

final case class Database(
    @describe("Database Host Name")
    host: Refined[String, NonEmpty],
    @describe("Database Port")
    port: Refined[Int, GreaterEqual[W.`1024`.T]]
) extends DataSource

final case class Kafka(
    @describe("Kafka Topics")
    topicName: String,
    @describe("Kafka Brokers")
    brokers: List[String]
) extends DataSource

object ZIOConfigExample extends ZIOAppDefault {
  import zio.config._
  import zio.config.refined._

  val json =
    s"""
       |"Database" : {
       |  "port" : "1024",
       |  "host" : "localhost"
       |}
       |""".stripMargin

  def run =
    for {
      _ <- ZIO.unit
      source = TypesafeConfigSource.fromHoconString(json)
      desc = descriptor[DataSource] from source
      dataSource <- read(desc)
      // Printing Auto Generated Documentation of Application Config
      _ <- Console.printLine(
        generateDocs(desc).toTable.toGithubFlavouredMarkdown
      )
      _ <- dataSource match {
        case Database(host, port) =>
          ZIO.debug(s"Start connecting to the database: $host:$port")
        case Kafka(_, brokers) =>
          ZIO.debug(s"Start connecting to the kafka brokers: $brokers")
      }
    } yield ()

}
```

There are many examples in [here](https://github.com/zio/zio-config/tree/master/examples/shared/src/main/scala/zio/config/examples) straight away as well.

Try out _ZIO Config_ quickly in [Scastie](https://scastie.scala-lang.org/WMlkdQeZQvm4yDyZ0pigJA), which comes pre-loaded with an example in scala-3. We try to make sure the scastie-buildsettings are updated with latest version of _ZIO Config_.
