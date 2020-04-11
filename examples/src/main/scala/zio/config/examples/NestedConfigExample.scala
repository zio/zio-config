package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ConfigDocs._
import zio.config._

object NestedConfigExample extends App {

  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database =
    (string("connection") |@| int("port"))(Database.apply, Database.unapply)

  val appConfig =
    (nested("south") { database } ?? "South details" |@|
      nested("east") { database } ?? "East details" |@|
      string("appName"))(AwsConfig, AwsConfig.unapply)

  // For simplicity in example, we use map source. Works with HOCON.
  val source =
    ConfigSource.fromMap(
      constantMap = Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      ),
      keyDelimiter = Some('.')
    )

  val runtime = zio.Runtime.default

  // Read
  assert(read(appConfig from source) == Right(AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp")))

  // Details Both Report of the nested configurations.
  assert(
    generateDocs(appConfig) ==
      Both(
        Both(
          NestedPath(
            "south",
            Both(
              NestedPath(
                "connection",
                Leaf(Sources(Set.empty), List("value of type string", "South details"))
              ),
              NestedPath("port", Leaf(Sources(Set.empty), List("value of type int", "South details")))
            )
          ),
          NestedPath(
            "east",
            Both(
              NestedPath(
                "connection",
                Leaf(Sources(Set.empty), List("value of type string", "East details"))
              ),
              NestedPath("port", Leaf(Sources(Set.empty), List("value of type int", "East details")))
            )
          )
        ),
        NestedPath("appName", Leaf(Sources(Set.empty), List("value of type string")))
      )
  )

  // Details with a peek at each value as well
  assert(
    generateDocsWithValue(appConfig, AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp")) ==
      Right(
        Both(
          Both(
            NestedPath(
              "south",
              Both(
                NestedPath(
                  "connection",
                  Leaf(
                    Sources(Set.empty),
                    List("value of type string", "South details"),
                    Some("abc.com")
                  )
                ),
                NestedPath(
                  "port",
                  Leaf(
                    Sources(Set.empty),
                    List("value of type int", "South details"),
                    Some("8111")
                  )
                )
              )
            ),
            NestedPath(
              "east",
              Both(
                NestedPath(
                  "connection",
                  Leaf(
                    Sources(Set.empty),
                    List("value of type string", "East details"),
                    Some("xyz.com")
                  )
                ),
                NestedPath(
                  "port",
                  Leaf(
                    Sources(Set.empty),
                    List("value of type int", "East details"),
                    Some("8888")
                  )
                )
              )
            )
          ),
          NestedPath(
            "appName",
            Leaf(Sources(Set.empty), List("value of type string"), Some("myApp"))
          )
        )
      )
  )

  // Write your nested config back.

  import PropertyTree._

  assert(
    write(appConfig, AwsConfig(Database("abc.com", 8111), Database("xyz.com", 8888), "myApp")) ==
      Right(
        Record(
          Map(
            "south"   -> Record(Map("connection" -> Leaf("abc.com"), "port" -> Leaf("8111"))),
            "east"    -> Record(Map("connection" -> Leaf("xyz.com"), "port" -> Leaf("8888"))),
            "appName" -> Leaf("myApp")
          )
        )
      )
  )
}
