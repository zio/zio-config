---
id: doc_index
title:  "Documentation"
---

# Example: Generating a Configuration Manual

To generate the documentation of the config, call `generateDocs`. 

```scala
  generateDocs(config)
```

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    (nested("south") { database } ? "South details" |@|
      nested("east") { database } ? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

   generateDocs(config)
```

yields the result `ConfigDocs[String, String]`:

```scala
      And(
        And(
          NestedConfig(
            "south",
            And(
              PathDetails("connection", Descriptions(List("value of type string", "South details"))),
              PathDetails("port", Descriptions(List("value of type int", "South details")))
            )
          ),
          NestedConfig(
            "east",
            And(
              PathDetails("connection", Descriptions(List("value of type string", "East details"))),
              PathDetails("port", Descriptions(List("value of type int", "East details")))
            )
          )
        ),
        PathDetails("appName", Descriptions(List("value of type string")))
      )
```

### More detail
`And(left, right)` means the `left` and `right` should exist in the config. For the same reason we have
`NestedConfig`, `Or` etc, that are nodes of `ConfigDocs[K,V]`. `K` means, the value of `key` and `V` is
the type of the value before it gets parsed.
