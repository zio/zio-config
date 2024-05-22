---
id: read-from-various-sources
title:  "Read from various Sources"
---

ZIO Config supports various sources for reading configurations. In this guide, we will see how to read configurations from different sources such as in-memory maps, HOCON strings, files, YAML, and XML.

## In-memory Map Source

To load configs from an in-memory map, you can use `ConfigProvider.fromMap` method:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/InmemoryMapSourceExample.scala")
```

## Typesafe (HOCON) Config Source

To enable HOCON source, we have to add the `zio-config-typesafe` module to our dependencies in `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
```

By importing the `zio.config.typesafe._` module, we can read configs from HOCON sources.

### HOCON String

We can use `ConfigProvider.fromHoconString` to load configs from a HOCON string:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/TypesafeHoconStringSourceExample.scala")
```

### HOCON File

Similar to the above example, we can read configs from a HOCON file, using `ConfigProvider.fromHoconFile`. 

Assume we have a HOCON file `application.simple.conf` in the `resources` directory:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/resources/application.simple.conf")
```

We can read the configuration file as follows:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/TypesafeHoconFileSourceExample.scala")
```

### JSON File

We can use `zio-config-typesafe` module to fetch json as well. So let's add it to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config-typesafe" % "@VERSION@"
```

Assume we have a JSON file `application.json` in the `resources` directory:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/resources/application.json")
```

We can read the configuration file as follows:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/TypesafeJsonFileSourceExample.scala")
```

## YAML Source

Let's add these four lines to our `build.sbt` file as we are using these modules in our examples:

```scala
libraryDependencies += "dev.zio" %% "zio-config"          % "<version>"
libraryDependencies += "dev.zio" %% "zio-config-yaml"     % "<version>" // for reading yaml configuration files
libraryDependencies += "dev.zio" %% "zio-config-magnolia" % "<version>" // for deriving configuration descriptions
```

Assume we have the following configuration file:

```scala
import utils._
printSource("examples/shared/src/main/resources/application.yml")
```

We can read the configuration file as follows:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/YamlConfigReaderExample.scala")
```

Here is the output:

```
bootstrapServers: List(localhost:9092, locathost:9094)
region: US
port: 100
```

## XML

ZIO Config can read XML strings using the `zio-config-yaml` module, so we have to add the following line to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-config-yaml" % "@VERSION@"
```

Note that it's experimental with a dead simple native xml parser, 
Currently it cannot XML comments, and has not been tested with complex data types, which will be fixed in the near future.

Assume we have the `application.xml` file in the `resources` directory:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/resources/application.xml")
```

We can load the configuration file as follows:

```scala mdoc:passthrough
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/configsources/YamlConfigReaderExample.scala")
```

## Indexed Map, Array datatype, and a some implementation notes

`zio-config` comes up with the idea of `IndexedFlat` allowing you to define indexed configs (see examples below).
However, the constructors of `IndexedFlat` is not exposed to the user for the time being, since it can conflate with some ideas in `zio.core` `Flat`,
and resulted in failures whenever `IndexedFlat` was converted to a `Flat` internally. Example: https://github.com/zio/zio-config/issues/1095

Therefore, some of these ideas around `Indexing` is  pushed back to `ZIO` and incorporated within the `Flat` structure.

See https://github.com/zio/zio/pull/7823 and https://github.com/zio/zio/pull/7891

These changes are to keep the backward compatibility of ZIO library itself.

### What does it mean to users?
It implies, for sequence (or list) datatypes, you can use either `<nil>` or `""` to represent empty list in a flat structure.
See the below example where it tries to mix indexing into flat structure.
We recommend using `<nil>` over `""` whenever you are trying  to represent a real indexed format

Example:

```scala mdoc:compile-only
import zio._
import zio.config._, magnolia._

final case class Department(name: String, block: Int)

final case class Employee(departments: List[Department], name: String)
final case class Config(employees: List[Employee])

val map =
  Map(
    "employees[0].name" -> "jon",
    "employees[0].departments[0].name" -> "science",
    "employees[0].departments[0].block" -> "10",
    "employees[0].departments[1].name" -> "maths",
    "employees[0].departments[2].block" -> "11",
    "employees[1].name" -> "foo",
    "employees[1].departments" -> "<nil>",
  )

ConfigProvider.fromMap(map).load(deriveConfig[Config])
```

Although we support indexing within Flat, formats such as Json/HOCON/XML is far better to work with indexing,
and zio-config supports these formats making use of the above idea.


### Another simple example of an indexed format

```scala mdoc:compile-only
import zio._
import zio.config._, magnolia._

final case class Employee(age: Int, name: String)

 val map = 
   Map(
     "department.employees[0].age" -> "10",
     "department.employees[0].name" -> "foo",
     "department.employees[1].age" -> "11",
     "department.employees[1].name" -> "bar",
     "department.employees[2].age" -> "12",
     "department.employees[2].name" -> "baz",
   )


val provider = ConfigProvider.fromMap(map)
val config = Config.listOf("employees", deriveConfig[Employee]).nested("department")
val result = provider.load(config)
```
