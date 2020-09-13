package zio.config.examples

import zio.config._, ConfigDescriptor._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    nested("database") {
      (int("PORT") ?? "Example: 8088" |@|
        string("URL").optional ?? "Example: abc.com")(Database.apply, Database.unapply) ?? "Database related"
    }

  val docs =
    generateDocs(config)

  val markdown =
    docs.toTable.asGithubFlavouredMarkdown

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

  val confluenceMarkdown =
    docs.toTable.asConfluenceMarkdown(None)

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
