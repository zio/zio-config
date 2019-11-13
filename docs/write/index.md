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
