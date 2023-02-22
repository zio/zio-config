---
id: read-from-various-sources
title:  "Read from various Sources"
---

zio-config supports various sources.
More documentation around `IndexedFlat` (an extension of  ZIO's  `ConfigProvider.Flat`)
to handle more complex sources will be provided soon.

Forming a source gets into a standard pattern, and is easy for you to add another one.

```scala mdoc:silent
import zio.IO
import zio.config._, Config._, ConfigSource._
import zio.config.magnolia._
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

More details about defining config descriptor is in [here](manual-creation-of-config-descriptor.md).

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

Alternatively you can follow below snippet, yielding Config[MyConfig], which you can use as ZIO environment.

```scala mdoc:silent
ZConfig.fromMap(Map(), myConfig)
// yielding Config[MyConfig], which is a service of config that you can use as ZIO environments.
```

## Multi Map Source

This support a list of values for a key.

```scala mdoc:silent
case class ListConfig(ldap: String, port: List[Int], dburl: String)

val listConfig = (string("LDAP") zip listOf("PORT")(int) zip string("DB_URL")).to[ListConfig]

val multiMapSource =
  ConfigSource.fromMultiMap(
    Map(
      "LDAP" -> ::("xyz",  Nil),
      "PORT" -> ::("1222" , "2221" :: Nil),
      "DB_URL" -> ::("postgres",  Nil)
    )
  )

read(myConfig from multiMapSource)
// Running this to completion yields ListConfig(xyz, List(1222, 2221), postgres)
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

Similar to Hocon source, the only difference is `ConfigSource.fromYamlString`
