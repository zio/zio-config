---
id: report_index
title:  " Report"
---

You need this import everywhere

```scala mdoc:silent
import zio.config._, ConfigDescriptor._

```

Calling `generateDocs` can give some documentation (man page).
But most often, we need these docs to act as a report that holds the value of the actual config parameter
along with the rest of the details. 

It is as simple as 

```scala mdoc:silent
case class MyConfig(username: String, password: String)

val config = 
  (string("USERNAME") |@| string("PASSWORD"))(MyConfig.apply, MyConfig.unapply)

// Note that, we got MyConfig by reading the config. As of now we are using a constant
val value: MyConfig = MyConfig("XYZ", "ABC")   

generateDocsWithValue(config, value)
```

This is like a configuration manual, except it also shows the values for each documentation node.

```scala mdoc:silent
  case class Database(url: String, port: Int)
  case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    (nested("south") { database } ? "South details" |@|
      nested("east") { database } ? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

  // For simplicity in example, we use map source. Works with hoccon.
  val source =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )

 
  // Note that we got AwsConfig from reading the config
  val result: AwsConfig = AwsConfig(Database("xyz", 100), Database("xxx", 111), "MyApp")

  generateDocsWithValue(appConfig, result)
```

yields the result:

```scala mdoc:silent
import ConfigDocs.Details._, ConfigDocs._

Right(
  Both(
    Both(
      NestedPath(
        "south",
        Both(
          Path(
            "connection",
            DescriptionsWithValue(Some("abc.com"), List("value of type string", "South details"))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8111"), List("value of type int", "South details"))
          )
        )
      ),
      NestedPath(
        "east",
        Both(
          Path(
            "connection",
            DescriptionsWithValue(Some("xyz.com"), List("value of type string", "East details"))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8888"), List("value of type int", "East details"))
          )
        )
      )
    ),
    Path("appName", DescriptionsWithValue(Some("myApp"), List("value of type string")))
  )
)
```

### More detail
`Both(left, right)` means the `left` and `right` should exist in the config. For the same reason we have
`NestedPath`, `Or` etc, that are nodes of `ConfigDocs[K,V]`. `K` means, the value of `key` and `V` is
the type of the value before it gets parsed.