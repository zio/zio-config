---
id: write_index
title:  "Write Config"
---

# Example: Writing Configuration

To write a configured value back:

```scala
  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database: ConfigDescriptor[Database] =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig: ConfigDescriptor[AwsConfig] =
    ((nested("south") { database } ? "South details" |@|
      nested("east") { database } ? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)) ? "asdf"

  val awsConfig: AwsConfig = ...

  write(appConfig, awsConfig)
```

yields

```scala
Right(
    Record(
      Map(
        "south" ->
          Record(
            Map(
              "connection" -> Leaf[String, String]("abc.com"),
              "port"       -> Leaf[String, String]("8111")
            )
          ),
        "east" ->
          Record(
            Map(
              "connection" -> Leaf[String, String]("xyz.com"),
              "port"       -> Leaf[String, String]("8888")
            )
          ),
        "appName" -> Leaf[String, String]("myApp")
      )
    )
  )
```