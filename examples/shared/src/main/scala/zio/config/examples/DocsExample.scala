package zio.config.examples

import zio.config._

import Config._
import zio.Config
import zio.config.syntax._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config: Config[Database] =
    ((Config.int("PORT") ?? "Example: 8088" zip
      Config.string("URL").optional ?? "Example: abc.com").to[Database] ?? "Database related").nested("database")

  val docs: ConfigDocs =
    generateDocs(config)

  val markdown =
    docs.toTable.toGithubFlavouredMarkdown

  println(markdown)

  val confluenceMarkdown: String =
    docs.toTable.toConfluenceMarkdown(None)

  println(confluenceMarkdown)

  /**
   *  |FieldName           |Format            |Description     |Sources|
   *  |---                 |---               |---             |---    |
   *  |[database](database)|[all-of](database)|Database related|       |
   *  ### database
   *  |FieldName|Format   |Description     |Sources|
   *  |---      |---      |---             |---    |
   *  |PORT     |recursion|Example: 8088   |       |
   *  |URL      |recursion|Example: abc.com|       |
   *  ## Configuration Details
   *  |FieldName          |Format           |Description     |Sources|
   *  |---                |---              |---             |---    |
   *  |[database|database]|[all-of|database]|Database related|       |
   *  ### database
   *  |FieldName|Format   |Description     |Sources|
   *  |---      |---      |---             |---    |
   *  |PORT     |recursion|Example: 8088   |       |
   *  |URL      |recursion|Example: abc.com|       |
   */
}
