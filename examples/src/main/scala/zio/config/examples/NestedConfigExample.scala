package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.config.examples.typesafe.EitherImpureOps

object NestedConfigExample extends App with EitherImpureOps {

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

  val readConfig =
    read(appConfig from source).loadOrThrow

  // Read
  assert(
    readConfig == AwsConfig(
      Database("abc.com", 8111),
      Database("xyz.com", 8888),
      "myApp"
    )
  )

  // Write your nested config back.
  val writtenResult: PropertyTree[String, String] =
    write(appConfig, readConfig).loadOrThrow

  assert(
    writtenResult ==
      PropertyTree.Record(
        Map(
          "south" -> PropertyTree.Record(
            Map(
              "connection" -> PropertyTree.Leaf("abc.com"),
              "port"       -> PropertyTree.Leaf("8111")
            )
          ),
          "east" -> PropertyTree.Record(
            Map(
              "connection" -> PropertyTree.Leaf("xyz.com"),
              "port"       -> PropertyTree.Leaf("8888")
            )
          ),
          "appName" -> PropertyTree.Leaf("myApp")
        )
      )
  )

  // Writing the tree back to Map
  assert(
    writtenResult.flattenString() ==
      Map(
        "east.port"        -> List("8888"),
        "appName"          -> List("myApp"),
        "east.connection"  -> List("xyz.com"),
        "south.port"       -> List("8111"),
        "south.connection" -> List("abc.com")
      )
  )

  // Let's write them back as hocon which is more prefered over representing it as a map
  /**
   *  Result:
   *  {{{
   *
   *    appName=myApp
   *    east {
   *       connection="xyz.com"
   *       port="8888"
   *    }
   *   south {
   *     connection="abc.com"
   *     port="8111"
   *   }
   *
   *  }}}
   */
  // Details Both Report of the nested configurations.

  assert(
    generateDocs(appConfig from source).toTable.asGithubFlavouredMarkdown ==
      s"""
         |## Configuration Details
         |
         |
         ||FieldName|Format                     |Description|Sources|
         ||---      |---                        |---        |---    |
         ||         |[all-of](fielddescriptions)|           |       |
         |
         |### Field Descriptions
         |
         ||FieldName     |Format         |Description         |Sources |
         ||---           |---            |---                 |---     |
         ||[south](south)|[all-of](south)|                    |        |
         ||[east](east)  |[all-of](east) |                    |        |
         ||appName       |primitive      |value of type string|constant|
         |
         |### south
         |
         ||FieldName |Format   |Description                        |Sources |
         ||---       |---      |---                                |---     |
         ||connection|primitive|value of type string, South details|constant|
         ||port      |primitive|value of type int, South details   |constant|
         |
         |### east
         |
         ||FieldName |Format   |Description                       |Sources |
         ||---       |---      |---                               |---     |
         ||connection|primitive|value of type string, East details|constant|
         ||port      |primitive|value of type int, East details   |constant|
         |""".stripMargin
  )
}
