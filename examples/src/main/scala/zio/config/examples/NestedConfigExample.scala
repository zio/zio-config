package zio.config.examples

import zio.config._
import Config._
import zio.DefaultRuntime
import zio.config.PropertyTree.{ Leaf, Record }
import zio.config.actions.ConfigDocs._

object NestedConfigExample extends App {

  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    (nested("south") { database } ? "South details" |@|
      nested("east") { database } ? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

  val source: ConfigSource[String, String] =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "south.com",
        "east.connection"  -> "east.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )

  val runtime = new DefaultRuntime {}

  // Read
  val result = runtime.unsafeRun(read(appConfig).provide(source))
  assert(result == AwsConfig(Database("south.com", 8111), Database("east.com", 8888), "myApp"))

  // Docs
  val docss = docs(appConfig, Some(result))

  assert(docss == {
    And(
      And(
        And(
          PathDetails(List("south", "connection"), Some("south.com"), List("value of type string", "South details")),
          PathDetails(List("south", "port"), Some("8111"), List("value of type int", "South details"))
        ),
        And(
          PathDetails(List("east", "connection"), Some("east.com"), List("value of type string", "East details")),
          PathDetails(List("east", "port"), Some("8888"), List("value of type int", "East details"))
        )
      ),
      PathDetails(List("appName"), Some("myApp"), List("value of type string"))
    )
  })

  // A write that keeps the nesting. Regardless of the source  where it came from,
  // user can choose to write by flatten it or keep it as it is, as a json, hoccon etc.
  assert(
    write(appConfig, result) ==
      Right(
        Record(
          Map(
            "south" ->
              Record(
                Map("connection" -> Leaf("south.com"), "port" -> Leaf("8111"))
              ),
            "east" ->
              Record(
                Map("connection" -> Leaf("east.com"), "port" -> Leaf("8888"))
              ),
            "appName" -> Leaf("myApp")
          )
        )
      )
  )
}
