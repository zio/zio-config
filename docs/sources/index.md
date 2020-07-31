---
id: sources_index
title:  "Read from various Sources"
---

zio-config supports various sources ranging from an in-memory Map, to environment variables, through to a HOCON file.
Forming a source gets into a standard pattern, and is easy for you to add another one.

```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._, ConfigSource._

```

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```

To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent

val myConfig =
  (string("LDAP") |@| int("PORT")|@| string("DB_URL"))(MyConfig.apply, MyConfig.unapply)

 // val automatedConfig = descriptor[MyConfig]; using zio-config-magnolia

```

More details about defining config descriptor is in [here](../configdescriptor/index.md).


## Constant Map

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

ZConfig.fromMap(Map(), myConfig)
// yielding Config[MyConfig], which is a service of config that you can use as ZIO environments.


```

## Multi Map Source

This support a list of values for a key.

```scala mdoc:silent
case class ListConfig(ldap: String, port: List[Int], dburl: String)

val listConfig = (string("LDAP") |@| list("PORT")(int) |@| string("DB_URL"))(ListConfig.apply, ListConfig.unapply)

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

ZConfig.fromMultiMap(Map(), myConfig, "constant")
// yielding Config[MyConfig], which is a service of config that you can use as ZIO environments.

```

## System Environment

```scala mdoc:silent

val sysEnvSource =
  ConfigSource.fromSystemEnv

// If you want to support list of values, then you should be giving a valueDelimiter
val sysEnvSourceSupportingList =
  ConfigSource.fromSystemEnv(keyDelimiter = None, valueDelimiter = Some(','))

// If you want to consider system-env as a nested config, provide keyDelimiter. Refer to API docs
// Example, Given KAFKA_SERVERS = "servers1, server2"
  ConfigSource.fromSystemEnv(keyDelimiter = Some('_'), valueDelimiter = Some(','))


```


Provide keyDelimiter if you need to consider flattened config as a nested config.
Provide valueDelimiter if you need any value to be a list

Example:

```
Given:

{{{
  property      = "KAFKA.SERVERS" = "server1, server2" ; "KAFKA.SERIALIZERS" = "confluent"
  keyDelimiter   = Some('.')
  valueDelimiter = Some(',')
}}}

```
then, the below config will work

```scala
nested("KAFKA")(string("SERVER") |@| string("FLAG"))(KafkaConfig.apply, KafkaConfig.unapply)
```


Give valueDelimiter =  `,`
and environment with `PORT=1222,2221`; then reading config yields
`ListConfig(xyz, List(1222, 2221), postgres)`

## System Properties

zio-config can source system properties.

```scala mdoc:silent

val sysPropertiesSource =
  ConfigSource.fromSystemProperties

// If you want to support list of values, then you should be giving a valueDelimiter
val sysPropertiesSourceWithList =
  ConfigSource.fromSystemProperties(None, valueDelimiter = Some(','))

// If you want to consider system-properties as a nested config, provide keyDelimiter. Refer to API doc
// Example, Given KAFKA.SERVERS = "servers1, server2"
  ConfigSource.fromSystemProperties(keyDelimiter = Some('.'), valueDelimiter = Some(','))


```
Give valueDelimiter =  `,`
and environemnt with `PORT=1222,2221`; then reading config yields
`ListConfig(xyz, List(1222, 2221), postgres)`


## Java Properties

```scala mdoc:silent

val javaProperties: java.util.Properties = new java.util.Properties() // Ideally loaded with values

val javaPropertiesSource =
  ConfigSource.fromProperties(javaProperties)

read(myConfig from javaPropertiesSource)

// If you want to support list of values, then you should be giving a valueDelimiter
val javaPropertiesSourceWithList =
  ConfigSource.fromProperties(javaProperties, valueDelimiter = Some(','))
```

## Properties File

```scala mdoc:silent

ZConfig.fromPropertiesFile("filepath", myConfig)

// yielding Config[MyConfig] which you provide to
// functions with zio environment as Config[MyConfig]

```

## HOCON String

To enable HOCON source, you have to bring in `zio-config-typesafe` module.
There are many examples in examples module in zio-config.

Here is an quick example

```scala mdoc:silent

import zio.config.typesafe._, TypesafeConfigSource._
import zio.config.magnolia.DeriveConfigDescriptor._

```

```scala mdoc:silent

case class SimpleConfig(port: Int, url: String, region: Option[String])

val automaticDescription = descriptor[SimpleConfig]

val hoconSource =
  TypesafeConfigSource.fromHoconString(
      """
      {
        port : 123
        url  : bla
        region: useast
      }

      """
    )


val anotherHoconSource =
  TypesafeConfigSource.fromHoconString(
      """
        port=123
        url=bla
        region=useast
      """
  )

hoconSource match {
  case Left(value) => Left(value)
  case Right(source) => read(automaticDescription from source)
}

// yielding Right(SimpleConfig(123,bla,Some(useast)))

anotherHoconSource match {
  case Left(value) => Left(value)
  case Right(source) => read(automaticDescription from source)
}

// yielding Right(SimpleConfig(123,bla,Some(useast)))

// Please check other ways to load the hocon file in `TypesafeConfig`

// You could also do, in which case the return type is `Config` service
TypesafeConfig.fromHoconString(
     """
      {
        port : 123
        url  : bla
        region: useast
      }
      """, automaticDescription)


```


## HOCON File

Similar to `TypesafeConfig.fromHoconString(str, automaticDescription)`

```scala mdoc:silent

TypesafeConfig.fromHoconFile(new java.io.File("fileapth"), automaticDescription)

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

TypesafeConfig.fromHoconString(jsonString, automaticDescription)


```
Please check other ways to load the hocon file in `TypesafeConfig`

## Command Line Arguments

This is currently experimental.

### Simple
```scala mdoc:silent
case class SimpleCommandLineConfig(key1: String, key2: String)

val simpleCmdLineArgs = "--key1 value1 --key2 value2"
val simpleSource = ConfigSource.fromCommandLineArgs(simpleCmdLineArgs.split(' ').toList)
val simpleConfig = descriptor[SimpleCommandLineConfig] from simpleSource
```
### Nested: Approach 1

```scala mdoc:silent
case class SparkConf(key1: String, key2: String)
case class NestedCommandLineConfig(conf: SparkConf, key3: String)

val nestedCmdLineArgs = "--conf.key1 v1 --conf.key2 v2 --key3 v3"
 
val nestedSource = 
   ConfigSource.fromCommandLineArgs(
   nestedCmdLineArgs.split(' ').toList, 
   keyDelimiter = Some('.')
)

val nestedConfig = descriptor[NestedCommandLineConfig] from nestedSource
assert(read(nestedConfig) == Right(NestedCommandLineConfig(SparkConf("v1", "v2"), "v3")))

```

This config is for those developers who really used to system properties `(-Dconf.key=1)` and want to take the same approach towards command line arguments.
Here we make use of delimiter `.` as the tool to nesting.

For those who hate delimited keys in command line arguments and the associated nesting, we will have different approach as given below

### Nested: Approach 2

```scala mdoc:silent

val nestedCmdLineArgs2 = "--conf -key1=v1 --conf -key2=v2 --key3 v3"
val nestedSource2 = ConfigSource.fromCommandLineArgs(nestedCmdLineArgs2.split(' ').toList)
val nestedConfig2 = descriptor[NestedCommandLineConfig] from nestedSource2

assert(read(nestedConfig2) == Right(NestedCommandLineConfig(SparkConf("v1", "v2"), "v3")))
```

Here we don't use delimiters for nesting, hence keyDelimiter is `None`. 
In this case any key-value that comes after `--conf` comes under the root path conf. This is followed in various places such as `SparkConf`.

In fact, we can go any level nesting. For example, we can give `---aws --db -url="v" ---aws --kinesis -topic=x`,
although let's don't complicate our command line arguments.

### Map
Both the approaches that we saw with nesting is applicable to `map`.

```scala mdoc:silent

val mapArgs = "--conf.key1=value1  --conf.key2=value2"

```

`map("conf")(string)` retrieving `Map("key1" -> "value1", "key2" -> "value2")`.  

This will also work if `mapArgs` is `--conf -key1=value1 --conf -key2=value2`.

### List: Approach 1

```scala mdoc:silent

val listArgs = "--users Jane --users Jack"
val listSource = ConfigSource.fromCommandLineArgs(listArgs.split(' ').toList)
val listConfigCmdLineArgs = list("users")(string) from listSource

assert(read(listConfigCmdLineArgs) == Right(List("Jane", "Jack")))

```

### Lists: Approach 2

```scala mdoc:silent

val listArgs2 = "--users Jane,Jack"

// args.split(' ') is only for demo purpose. We already get a list if we use zio.App

val listSource2 = ConfigSource.fromCommandLineArgs(
   listArgs2.split(' ').toList,
   valueDelimiter = Some(',')
)

assert(read(list("users")(string) from listSource2) == Right(List("Jane", "Jack")))

```

### Behaviour of List in various sources

No single values will be regarded as list. This is based on feedback from users.

For the config:
 
```
Case class Config(key: List[String]) 
```

If the source is below HOCON (or json)

```scala
 {
   Key : value
 }
```

then it fails, saying a `Sequence` is expected. This is quite intuitive but worth mentioning for users who are new to HOCON.

However the following configs will work, as it clearly indicate it is a `List` with square brackets 
```scala
{
   Key: [value]
}
```
```scala

{
   key: [value1, value2]
}


```

If the source is a map given below (for example, in system environment), then it succeeds given any delimiter
as it contains only one single value.

```scala
export key="value"
``` 

Given `valueDelimiter=Some(',')` the following config will work and we are able to retrieve List(value1, value2)

```scala
export key="value1, value2"

```

### A Production application config using command line arguments (demo)

```scala mdoc:silent

case class UserPassword(username: String, password: String)
case class DatabaseConfig(database: UserPassword, url: String)
case class VaultConfig(userPassword: UserPassword)
case class SparkConfig(databaseConfig: DatabaseConfig, numexecs: Int)
case class AppConfig(conf: SparkConfig, vault: VaultConfig, users: List[String], region: List[String])

val complexArgs = "--conf.database.username=Foo --conf.database.password=Bar --conf.database.url=jdbc://xyz --conf.numexecs=10 --vault.username=Foo2 --vault.password=Bar2 --users Jane --users Jack --region TW,US"

val complexSource = ConfigSource.fromCommandLineArgs(
  complexArgs.split(' ').toList,
  Some('.'),
  Some(',')
)
val appConfig = read(descriptor[AppConfig] from complexSource)

```
