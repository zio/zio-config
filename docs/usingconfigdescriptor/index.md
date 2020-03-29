---
id: configdescriptorusage_index
title:  "Using ConfigDescriptor"
---

## Using Config Descriptor

Given a single [ConfigDescriptor](../configdescriptor/index.md) we can use it to:

1. Get Readers that can read config from various sources
2. Get Writer that can write config back
3. Document the config
4. Create report on the config


## Reader from Config Descriptor

You should be familiar with reading config from various sources, given a  config descriptor.

```scala mdoc:silent
import zio.IO
import zio.config._, ConfigDescriptor._
import zio.config.PropertyTree._
import zio.config.ConfigDocs._, ConfigDocs.Details._

```

```scala mdoc:silent

case class MyConfig(ldap: String, port: Int, dburl: String)

```

To perform any action using zio-config, we need a configuration description.
Let's define a simple one.


```scala mdoc:silent

val myConfig =
  (string("LDAP") |@| int("PORT")|@| string("DB_URL"))(MyConfig.apply, MyConfig.unapply)

read(myConfig from ConfigSource.fromMap(Map()))

```

## Writer from config descriptor


To write a configured value back:

```scala mdoc:silent
import zio.Runtime

  case class Database(url: String, port: Int)
  case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val map =
    Map(
      "south.connection" -> "abc.com",
      "south.port" -> "8111",
      "east.connection" -> "xyz.com",
      "east.port" -> "8888",
      "appName" -> "myApp"
    )

  val appConfig =
    (((nested("south") { database } ?? "South details" |@|
      nested("east") { database } ?? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)) ?? "asdf"
    ) from ConfigSource.fromMap(map)

  val awsConfig =
    read(appConfig)

  val awsConfigReuslt: AwsConfig = Runtime.default.unsafeRun(awsConfig)
   // yields AwsConfig(Database(abc.com, 8111), Database(xyz.com, 8888), myApp)

write(appConfig, awsConfigReuslt)

// yields

 Right(
  Record(
    Map(
      "south"   -> Record(Map("connection" -> Leaf("abc.com"), "port" -> Leaf("8111"))),
      "east"    -> Record(Map("connection" -> Leaf("xyz.com"), "port" -> Leaf("8888"))),
      "appName" -> Leaf("myApp")
    )
  )
)

 // To yield the input map that was fed in, call `flattenString` !!
 write(appConfig, awsConfigReuslt).map(_.flattenString())

 // yields
  Right(
     Map(
      "south.connection" -> "abc.com",
      "south.port" -> "8111",
      "east.connection" -> "xyz.com",
      "east.port" -> "8888",
      "appName" -> "myApp"
    )
  )

```

## Document the config


To generate the documentation of the config, call `generateDocs`. 


```scala mdoc:silent

 generateDocs(appConfig)

  // yields the result `ConfigDocs[String, String]`:

 Both(
   Both(
     NestedPath(
       "south",
       Both(
         Path("connection", Descriptions(Sources(List("constant")), List("value of type string", "South details"))),
         Path("port", Descriptions(Sources(List("constant")), List("value of type int", "South details")))
       )
     ),
     NestedPath(
       "east",
       Both(
         Path("connection", Descriptions(Sources(List("constant")), List("value of type string", "East details"))),
         Path("port", Descriptions(Sources(List("constant")), List("value of type int", "East details")))
       )
     )
   ),
   Path("appName", Descriptions(Sources(List("constant")), List("value of type string")))
 )
```

#### More detail
`Both(left, right)` means the `left` and `right` should exist in the config. For the same reason we have
`NestedPath`, `Or` etc, that are nodes of `ConfigDocs[K,V]`. `K` means, the value of `key` and `V` is
the type of the value before it gets parsed.

We will see more better representation of the documentation in the immediate future versions of zio-config.


## Report on the config

Calling `generateDocs` can give some documentation (man page).
But most often, we need these docs to act as a report that holds the value of the actual config parameter
along with the rest of the details. 


```scala mdoc:silent

generateDocsWithValue(appConfig, awsConfigReuslt)

// yields the result:

Right(
  Both(
    Both(
      NestedPath(
        "south",
        Both(
          Path(
            "connection",
            DescriptionsWithValue(Some("abc.com"), Sources(List("constant")), List("value of type string", "South details"))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8111"), Sources(List("constant")), List("value of type int", "South details"))
          )
        )
      ),
      NestedPath(
        "east",
        Both(
          Path(
            "connection",
            DescriptionsWithValue(Some("xyz.com"), Sources(List("constant")), List("value of type string", "East details"))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8888"), Sources(List("constant")), List("value of type int", "East details"))
          )
        )
      )
    ),
    Path("appName", DescriptionsWithValue(Some("myApp"), Sources(List("constant")), List("value of type string")))
  )
)
```

#### More detail
`Both(left, right)` means the `left` and `right` should exist in the config. For the same reason we have
`NestedPath`, `Or` etc, that are nodes of `ConfigDocs[K,V]`. `K` means, the value of `key` and `V` is
the type of the value before it gets parsed.g
