package zio.config.examples

import zio.config._

import ConfigDescriptor._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config: ConfigDescriptor[Database] =
    nested("database") {
      (int("PORT") ?? "Example: 8088" zip
        string("URL").optional ?? "Example: abc.com").to[Database] ?? "Database related"
    }

  val docs: ConfigDocs =
    generateDocs(config)

  val markdown =
    docs.toTable.toGithubFlavouredMarkdown

  assert(
    markdown ==
      s"""
         |## Configuration Details
         |
         |
         ||FieldName           |Format            |Description     |Sources|
         ||---                 |---               |---             |---    |
         ||[database](database)|[all-of](database)|Database related|       |
         |
         |### database
         |
         ||FieldName|Format   |Description                                           |Sources|
         ||---      |---      |---                                                   |---    |
         ||PORT     |primitive|value of type int, Example: 8088                      |       |
         ||URL      |primitive|value of type string, optional value, Example: abc.com|       |
         |""".stripMargin
  )

  val confluenceMarkdown: String =
    docs.toTable.toConfluenceMarkdown(None)

  assert(
    confluenceMarkdown ==
      s"""
         |## Configuration Details
         |
         |
         ||FieldName          |Format           |Description     |Sources|
         ||---                |---              |---             |---    |
         ||[database|database]|[all-of|database]|Database related|       |
         |
         |### database
         |
         ||FieldName|Format   |Description                                           |Sources|
         ||---      |---      |---                                                   |---    |
         ||PORT     |primitive|value of type int, Example: 8088                      |       |
         ||URL      |primitive|value of type string, optional value, Example: abc.com|       |
         |""".stripMargin
  )
}
