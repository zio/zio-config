package zio.config.examples

import zio.config._
import ConfigDescriptor._, zio.config.ConfigDocs.Details._
import zio.config.ConfigDocs._

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
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
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
              Path(
                "connection",
                Descriptions(Sources(Nil), List("value of type string", "South details"))
              ),
              Path("port", Descriptions(Sources(Nil), List("value of type int", "South details")))
            )
          ),
          NestedPath(
            "east",
            Both(
              Path(
                "connection",
                Descriptions(Sources(Nil), List("value of type string", "East details"))
              ),
              Path("port", Descriptions(Sources(Nil), List("value of type int", "East details")))
            )
          )
        ),
        Path("appName", Descriptions(Sources(Nil), List("value of type string")))
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
                Path(
                  "connection",
                  DescriptionsWithValue(
                    Some("abc.com"),
                    Sources(Nil),
                    List("value of type string", "South details")
                  )
                ),
                Path(
                  "port",
                  DescriptionsWithValue(
                    Some("8111"),
                    Sources(Nil),
                    List("value of type int", "South details")
                  )
                )
              )
            ),
            NestedPath(
              "east",
              Both(
                Path(
                  "connection",
                  DescriptionsWithValue(
                    Some("xyz.com"),
                    Sources(Nil),
                    List("value of type string", "East details")
                  )
                ),
                Path(
                  "port",
                  DescriptionsWithValue(
                    Some("8888"),
                    Sources(Nil),
                    List("value of type int", "East details")
                  )
                )
              )
            )
          ),
          Path(
            "appName",
            DescriptionsWithValue(Some("myApp"), Sources(Nil), List("value of type string"))
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
