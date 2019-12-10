---
id: write_index
title:  "Write Config"
---

# Example: Writing Configuration

You need this import everywhere

```scala mdoc:silent
import zio.config._, ConfigDescriptor._

```

To write a configured value back:

```scala mdoc:silent
  case class Database(url: String, port: Int)
  case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    ((nested("south") { database } ?? "South details" |@|
      nested("east") { database } ?? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)) ?? "asdf"

   // Note that we got AwsConfig from reading the config
  val awsConfig: AwsConfig = AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "MyApp")

  write(appConfig, awsConfig)
```

yields

```scala mdoc:silent
import zio.config.PropertyTree._

 Right(
    Record(
      Map(
        "south" ->
          Record(
            Map(
              "connection" -> Leaf("abc.com"),
              "port"       -> Leaf("8111")
            )
          ),
        "east" ->
          Record(
            Map(
              "connection" -> Leaf("xyz.com"),
              "port"       -> Leaf("8888")
            )
          ),
        "appName" -> Leaf("myApp")
      )
    )
  )
```