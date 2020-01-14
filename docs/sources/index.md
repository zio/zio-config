---
id: sources_index
title:  "Sources"
---

zio-config supports various sources ranging from an in-memory Map, to environment variables, through to a HOCON file.
Forming a source gets into a standard pattern, and is easy for you to add another one.

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

 // val automatedConfig = description[MyConfig]; using zio-config-magnolia

```

More details about defining config descriptor is in [here](../configdescriptor/index.md).


## Constant Map Source

```scala mdoc:silent

val mapSource =
  ConfigSource.fromMap(
    Map(
      "LDAP" -> "xyz",
      "PORT" -> "1222",
      "DB_URL" -> "postgres"
    )
  )  

val io = read(myConfig from mapSource)
// Running io (which is a zio) to completion yields  MyConfig(xyz, 1222, postgres)

```

Alternatively you can follow below snippet,  yielding Config[MyConfig], which you can use as ZIO environment.

```scala mdoc:silent

Config.fromMap(Map(), myConfig)
// yielding Config[MyConfig], which is a service of config that you can use as ZIO environments.


```

## Multi Map Source

This support a list of values for a key.

```scala mdoc:silent
case class ListConfig(ldap: String, port: List[Int], dburl: String)

val listConfig = (string("LDAP") |@| list(int("PORT")) |@| string("DB_URL"))(ListConfig.apply, ListConfig.unapply)

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

Alternatively you can follow below snippet, yielding Config[MyConfig], which you can use as ZIO environment.

```scala mdoc:silent

Config.fromMultiMap(Map(), myConfig)
// yielding Config[MyConfig], which is a service of config that you can use as ZIO environments.

```

## System Env Source

```scala mdoc:silent

val sysEnvSource =
  ConfigSource.fromEnv(None)

// If you want to support list of values, then you should be giving a valueSeparator
val sysEnvSourceSupportingList = 
  ConfigSource.fromEnv(valueSeparator=Some(",")) 

```

Give valueSeparator =  `,` 
and environemnt with `PORT=1222,2221`; then reading config yields 
`ListConfig(xyz, List(1222, 2221), postgres)`

## System Properties

zio-config can source system properties.

```scala mdoc:silent

val sysPropertiesSource =
  ConfigSource.fromProperty(None)

// If you want to support list of values, then you should be giving a valueSeparator
val sysPropertiesSourceWithList = 
  ConfigSource.fromProperty(valueSeparator=Some(",")) 

```
Give valueSeparator =  `,` 
and environemnt with `PORT=1222,2221`; then reading config yields 
`ListConfig(xyz, List(1222, 2221), postgres)`


## Java Properties

```scala mdoc:silent

val javaProperties: java.util.Properties = new java.util.Properties() // Ideally loaded with values

val javaPropertiesSource =
  ConfigSource.fromJavaProperties(javaProperties, None)

read(myConfig from javaPropertiesSource)  

// If you want to support list of values, then you should be giving a valueSeparator
val javaPropertiesSourceWithList =
  ConfigSource.fromJavaProperties(javaProperties, valueSeparator = Some(","))
```

## Properties File Source

```scala mdoc:silent

Config.fromPropertyFile("filepath", myConfig)

// yielding Config[MyConfig] which you provide to 
// functions with zio environment as Config[MyConfig]

```

_Implementation Detail_: `Config.fromPropertyFile` simply yields `java.util.Properties` in a `ZIO` and delegate the rest to `ConfigSource.fromJavaProperties`

## HOCON String Source

To enable HOCON source, you have to bring in `zio-config-typesafe` module.
There are many examples in examples module in zio-config. 

Here is an quick example

```scala mdoc:silent

import zio.config.typesafe._, TypeSafeConfigSource._
import zio.config.magnolia.ConfigDescriptorProvider._

```

```scala mdoc:silent

case class SimpleConfig(port: Int, url: String, region: Option[String])

val automaticDescription = description[SimpleConfig]

val hoconSource =
  hocon(
    Right(
      """
      {
        port : 123
        url  : bla
        region: useast
      }
      
      """
    )
  )

val anotherHoconSource =
  hocon(
    Right(
      """
        port=123
        url=bla
        region=useast
      """
    )
  )

read(automaticDescription from hoconSource)
// yielding SimpleConfig(123,bla,Some(useast))


read(automaticDescription from anotherHoconSource)
// yielding SimpleConfig(123,bla,Some(useast))

// You could also do
TypesafeConfig.fromHoconString(
     """
      {
        port : 123
        url  : bla
        region: useast
      }
      """, automaticDescription)


```

## HOCON File Source

Similar to `TypesafeConfig.fromHoconString(str, automaticDescription)`

```scala mdoc:silent

TypesafeConfig.fromHoconFile(automaticDescription, new java.io.File("fileapth"))

```


## Json Source
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
    

TypesafeConfig.fromHoconString(jsonString, automaticDescription)


```