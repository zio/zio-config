package zio.config.examples

import zio.config._

import zio.Config, Config._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config: Config[Database] =
    ((Config.int("PORT") ?? "Example: 8088" zip
      Config.string("URL").optional ?? "Example: abc.com").to[Database] ?? "Database related").nested("database")

  val docs: ConfigDocs =
    generateDocs(config)

  val confluenceMarkdown: String =
    docs.toTable.toConfluenceMarkdown(None)

  println(confluenceMarkdown)

  /**
   *  ## Configuration Details
   *  |FieldName          |Format           |Description     |Sources|
   *  |---                |---              |---             |---    |
   *  |[database|database]|[all-of|database]|Database related|       |
   *  ### database
   *  |FieldName|Format   |Description     |Sources|
   *  |---      |---      |---             |---    |
   *  |PORT     |primitive|Example: 8088   |       |
   *  |URL      |primitive|Example: abc.com|       |
   */
}
