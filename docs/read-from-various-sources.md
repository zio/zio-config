---
id: read-from-various-sources
title:  "Read from various Sources"
---

zio-config supports various sources.
More documentation around `IndexedFlat` (an extension of  ZIO's  `ConfigProvider.Flat`)
to handle more complex sources will be provided soon.

Forming a source gets into a standard pattern, and is easy for you to add another one.

```scala mdoc:silent
import zio._, Config._, ConfigProvider._
import zio.config._, magnolia._
```

```scala mdoc:silent
case class MyConfig(ldap: String, port: Int, dburl: String)
```

To perform any action using zio-config, we need a configuration description.
Let's define a simple one.

```scala mdoc:silent
val myConfig =
  (string("LDAP") zip int("PORT") zip string("DB_URL")).to[MyConfig]

 // val automatedConfig = deriveConfig[MyConfig]; using zio-config-magnolia
```

More details about defining config descriptor is in [here](manual-creation-of-config.md).

## Constant Map

```scala mdoc:silent

val mapSource =
  ConfigProvider.fromMap(
    Map(
      "LDAP" -> "xyz",
      "PORT" -> "1222",
      "DB_URL" -> "postgres"
    )
  )

val io = mapSource.load(myConfig)
// Running io (which is a zio) to completion yields  MyConfig(xyz, 1222, postgres)

```


## HOCON String

To enable HOCON source, you have to bring in `zio-config-typesafe` module.
There are many examples in examples module in zio-config.

Here is an quick example

```scala mdoc:silent
import zio.config.typesafe._
import zio.config.magnolia._
```

```scala mdoc:silent
case class SimpleConfig(port: Int, url: String, region: Option[String])

val automaticDescription = deriveConfig[SimpleConfig]

val hoconSource =
  ConfigProvider.fromHoconString(
      """
      {
        port : 123
        url  : bla
        region: useast
      }

      """
    )


val anotherHoconSource =
  ConfigProvider.fromHoconString(
      """
        port=123
        url=bla
        region=useast
      """
  )

hoconSource.load(deriveConfig[SimpleConfig])

// yielding SimpleConfig(123,bla,Some(useast))
```

## HOCON File

```scala mdoc:silent
ConfigProvider.fromHoconFile(new java.io.File("fileapth"))
```

## Json

You can use `zio-config-typesafe` module to fetch json as well

```scala mdoc:silent
val jsonString =
   """
   {
     "port" : "123"
     "url"  : "bla"
     "region": "useast"
   }

   """

ConfigProvider.fromHoconString(jsonString)
```

## Yaml FIle

Similar to Hocon source, we have `ConfigProvider.fromYamlString`

```scala
import zio.config.yaml._

ConfigProvider.fromYamlString

```

## Xml String

zio-config can read XML strings. Note that it's experimental with a dead simple native xml parser, 
Currently it cannot XML comments, and has not been tested with complex data types, which will be fixed in the near future.

```scala
import zio.config.xml.experimental._
import zio.Config

final case class Configuration(aws: Aws, database: Database)

object Configuration {
  val config: Config[Configuration] =
    Aws.config.nested("aws").zip(Database.config.nested("database")).to[Configuration].nested("config")

  final case class Aws(region: String, account: String)

  object Aws {
    val config: Config[Aws] = Config.string("region").zip(Config.string("account")).to[Aws]
  }
  final case class Database(port: Int, url: String)

  object Database {
    val config: Config[Database] = Config.int("port").zip(Config.string("url")).to[Database]
  }
}

val config =
  s"""
     |<config>
     |  <aws region="us-east" account="personal"></aws>
     |  <database port="123" url="some url"></database>
     |</config>
     |
     |""".stripMargin

val parsed = ConfigProvider.fromYamlString(config).load(Configuration.config)

```
