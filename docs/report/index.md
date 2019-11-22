---
id: report_index
title:  " Docs / Report config"
---

Calling `generateDocs` can give some documentation (man page).
But most often, we need these docs to act as a report that holds the value of the actual config parameter
along with the rest of the details. 

It is as simple as 

```scala
generateDocsWithValue(config, value)
```

This is like a configuration manual, except it also shows the values for each documentation node.

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

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

 
  val result: AwsConfig = ???

  generateDocsWithValue(appConfig, result)
```

yields the result:

```python
Right(
  And(
    And(
      NestedPath(
        "south",
        And(
          Path(
            "connection",
            DescriptionsWithValue(Some("abc.com"), Descriptions(List("value of type string", "South details")))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8111"), Descriptions(List("value of type int", "South details")))
          )
        )
      ),
      NestedPath(
        "east",
        And(
          Path(
            "connection",
            DescriptionsWithValue(Some("xyz.com"), Descriptions(List("value of type string", "East details")))
          ),
          Path(
            "port",
            DescriptionsWithValue(Some("8888"), Descriptions(List("value of type int", "East details")))
          )
        )
      )
    ),
    Path("appName", DescriptionsWithValue(Some("myApp"), Descriptions(List("value of type string"))))
  )
)
```

### More detail
`And(left, right)` means the `left` and `right` should exist in the config. For the same reason we have
`NestedPath`, `Or` etc, that are nodes of `ConfigDocs[K,V]`. `K` means, the value of `key` and `V` is
the type of the value before it gets parsed.