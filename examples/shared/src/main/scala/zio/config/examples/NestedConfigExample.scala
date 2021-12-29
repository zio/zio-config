package zio.config.examples

import zio.IO
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps

import ConfigDescriptor._

object NestedConfigExample extends App with EitherImpureOps {

  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database: ConfigDescriptor[Database] =
    (string("connection") zip int("port")).to[Database]

  val appConfig: ConfigDescriptor[AwsConfig] =
    (nested("south")(database) ?? "South details" zip
      nested("east")(database) ?? "East details" zip
      string("appName")).to[AwsConfig]

  // For simplicity in example, we use map source. Works with HOCON.
  val source: ConfigSource =
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

  val readConfig: IO[ReadError[String], AwsConfig] =
    read(appConfig from source)

  // Read
  assert(
    readConfig equalM AwsConfig(
      Database("abc.com", 8111),
      Database("xyz.com", 8888),
      "myApp"
    )
  )

  // Write your nested config back.
  val writtenResult: PropertyTree[String, String] =
    write(
      appConfig,
      AwsConfig(
        Database("abc.com", 8111),
        Database("xyz.com", 8888),
        "myApp"
      )
    ).loadOrThrow

  assert(
    writtenResult ==
      PropertyTree.Record(
        Map(
          "south"   -> PropertyTree.Record(
            Map(
              "connection" -> PropertyTree.Leaf("abc.com"),
              "port"       -> PropertyTree.Leaf("8111")
            )
          ),
          "east"    -> PropertyTree.Record(
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
    generateDocs(appConfig from source).toTable.toGithubFlavouredMarkdown ==
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
         ||[south](south)|[all-of](south)|South details       |        |
         ||[east](east)  |[all-of](east) |East details        |        |
         ||appName       |primitive      |value of type string|constant|
         |
         |### south
         |
         ||FieldName |Format   |Description         |Sources |
         ||---       |---      |---                 |---     |
         ||connection|primitive|value of type string|constant|
         ||port      |primitive|value of type int   |constant|
         |
         |### east
         |
         ||FieldName |Format   |Description         |Sources |
         ||---       |---      |---                 |---     |
         ||connection|primitive|value of type string|constant|
         ||port      |primitive|value of type int   |constant|
         |""".stripMargin
  )
}
