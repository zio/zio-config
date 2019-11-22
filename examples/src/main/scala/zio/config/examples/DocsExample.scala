package zio.config.examples

import zio.config._
import ConfigDescriptor._, zio.config.actions.ConfigDocs._
import zio.config.actions.ConfigDocs.PathDetails

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ? "Example: 8088" |@|
      string("URL").optional ? "Example: abc.com")(Database.apply, Database.unapply) ? "Database related"

  assert(
    generateDocs(config) ==
      And(
        PathDetails("PORT", Descriptions(List("value of type int", "Example: 8088", "Database related"))),
        PathDetails(
          "URL",
          Descriptions(List("value of type string", "optional value", "Example: abc.com", "Database related"))
        )
      )
  )

  assert(
    generateDocsWithValue(config, Database(1, Some("value"))) ==
      Right(
        And(
          PathDetails(
            "PORT",
            DescriptionsWithValue(
              Some("1"),
              Descriptions(List("value of type int", "Example: 8088", "Database related"))
            )
          ),
          PathDetails(
            "URL",
            DescriptionsWithValue(
              Some("value"),
              Descriptions(List("value of type string", "optional value", "Example: abc.com", "Database related"))
            )
          )
        )
      )
  )
}
