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
      string("appName").default("myApp"))(AwsConfig, AwsConfig.unapply)

  // For simplicity in example, we use map source. Works with hoccon.
  val southDetails: ConfigSource[String, String] =
    ConfigSource.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "south.port"       -> "8111"
      ),
      Some("south details")
    )

  val eastDetails: ConfigSource[String, String] =
    ConfigSource.fromMap(
      Map(
        "east.connection" -> "xyz.com",
        "east.port"       -> "8888"
      ),
      Some("east details")
    )

  val source = southDetails orElse eastDetails

  val runtime = new DefaultRuntime {}

  // Read
  val result = runtime.unsafeRun(read(appConfig).provide(source))
  println(result)
  assert(result == AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp"))

  // Docs and Report of the nested configurations.
  assert(docs(appConfig, Some(result)) == {
    And(
      And(
        And(
          PathDetails(
            Vector("south", "connection"),
            Some("abc.com"),
            None,
            List("value of type string", "South details")
          ),
          PathDetails(Vector("south", "port"), Some("8111"), None, List("value of type int", "South details"))
        ),
        And(
          PathDetails(
            Vector("east", "connection"),
            Some("xyz.com"),
            None,
            List("value of type string", "East details")
          ),
          PathDetails(Vector("east", "port"), Some("8888"), None, List("value of type int", "East details"))
        )
      ),
      PathDetails(Vector("appName"), Some("myApp"), None, List("value of type string", "default value: myApp"))
    )
  })

  // readWithConfigDocs
  val (result2, configDocs) = runtime.unsafeRun(readWithConfigDocs(appConfig).provide(source))

  assert(result2 == AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp"))

  assert(
    configDocs ==
      SourceDescription(
        "Scala Map - south details: with fallback source: Scala Map - east details (on all errors)",
        And(
          And(
            And(
              PathDetails(
                Vector("south", "connection"),
                Some("abc.com"),
                Some("Scala Map - south details"),
                List("value of type string", "South details")
              ),
              PathDetails(
                Vector("south", "port"),
                Some("8111"),
                Some("Scala Map - south details"),
                List("value of type int", "South details")
              )
            ),
            And(
              PathDetails(
                Vector("east", "connection"),
                Some("xyz.com"),
                Some("Scala Map - east details"),
                List("value of type string", "East details")
              ),
              PathDetails(
                Vector("east", "port"),
                Some("8888"),
                Some("Scala Map - east details"),
                List("value of type int", "East details")
              )
            )
          ),
          PathDetails(
            Vector("appName"),
            Some("myApp"),
            Some("Config descriptor"),
            List("value of type string", "default value: myApp")
          )
        )
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
  )
}
