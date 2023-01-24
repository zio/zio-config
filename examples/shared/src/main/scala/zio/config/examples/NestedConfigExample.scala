package zio.config.examples

import zio.IO
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.Config, Config._
import zio.config.syntax._
import zio.ConfigProvider

object NestedConfigExample extends App with EitherImpureOps {

  final case class Database(url: String, port: Int)
  final case class AwsConfig(c1: Database, c2: Database, c3: String)

  val database: Config[Database] =
    (string("connection") zip int("port")).to[Database]

  val appConfig: Config[AwsConfig] =
    (database.nested("south") ?? "South details" zip
      database.nested("east") ?? "East details" zip
      string("appName")).to[AwsConfig]

  // For simplicity in example, we use map source. Works with HOCON.
  val source: ConfigProvider =
    ConfigProvider.fromMap(
      Map(
        "south.connection" -> "abc.com",
        "east.connection"  -> "xyz.com",
        "east.port"        -> "8888",
        "south.port"       -> "8111",
        "appName"          -> "myApp"
      )
    )

  val runtime = zio.Runtime.default

  val readConfig: IO[Config.Error, AwsConfig] =
    read_(appConfig from source)

  // Read
  assert(
    readConfig equalM AwsConfig(
      Database("abc.com", 8111),
      Database("xyz.com", 8888),
      "myApp"
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

  println(generateDocs(appConfig).toTable.toGithubFlavouredMarkdown)

  // FIXME: connection and port seem to be not part of recursion
  /**
   * ## Configuration Details
   * |FieldName|Format                     |Description|Sources|
   * |---      |---                        |---        |---    |
   * |         |[all-of](fielddescriptions)|           |       |
   * ### Field Descriptions
   * |FieldName     |Format         |Description  |Sources|
   * |---           |---            |---          |---    |
   * |[south](south)|[all-of](south)|South details|       |
   * |[east](east)  |[all-of](east) |East details |       |
   * |appName       |recursion      |             |       |
   * ### south
   * |FieldName |Format   |Description|Sources|
   * |---       |---      |---        |---    |
   * |connection|recursion|           |       |
   * |port      |recursion|           |       |
   * ### east
   * |FieldName |Format   |Description|Sources|
   * |---       |---      |---        |---    |
   * |connection|recursion|           |       |
   * |port      |recursion|           |       |
   */
}
