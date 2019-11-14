package zio.config.examples

import zio.config._
import ConfigDescriptor.{ _ }
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

  // Read
  val result = runtime.unsafeRun(read(appConfig from source))
  assert(result == AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp"))

  // Docs and Report of the nested configurations.
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

  // Write your nested config back.
  assert(
    write(appConfig, result) ==
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
  )
}
