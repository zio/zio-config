---
id: report_index
title:  " Docs / Report config"
---

Calling `docs` and passing the description causes zio-config
to generate a *configuration report*. You can also pass an optional actual config value which will
give the value associated with each key as well. 

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

  val runtime = new DefaultRuntime {}

  // Read (refer to zio config)
  val result = runtime.unsafeRun(read(appConfig from source))
 
  docs(appConfig, Some(result))

```

yields the result:

```scala
 assert(
  docs(appConfig, Some(result)) ==
    And(
      And(
        NestedConfig(
          "south",
          And(
            PathDetails("connection", Some("abc.com"), List("value of type string", "South details")),
            PathDetails("port", Some("8111"), List("value of type int", "South details"))
          )
        ),
        NestedConfig(
          "east",
          And(
            PathDetails("connection", Some("xyz.com"), List("value of type string", "East details")),
            PathDetails("port", Some("8888"), List("value of type int", "East details"))
          )
        )
      ),
      PathDetails("appName", Some("myApp"), List("value of type string"))
    )
)
```
