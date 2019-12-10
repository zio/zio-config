package zio.config.examples

import zio.config._
import ConfigDescriptor._, zio.config.ConfigDocs._, Details._
import zio.config.ConfigDocs.Path

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ?? "Example: 8088" |@|
      string("URL").optional ?? "Example: abc.com")(Database.apply, Database.unapply) ?? "Database related"

  assert(
    generateDocs(config) ==
      Both(
        Path("PORT", Descriptions(List("<empty>", "value of type int", "Example: 8088", "Database related"))),
        Path(
          "URL",
          Descriptions(
            List("<empty>", "value of type string", "optional value", "Example: abc.com", "Database related")
          )
        )
      )
  )

  assert(
    generateDocsWithValue(config, Database(1, Some("value"))) ==
      Right(
        Both(
          Path(
            "PORT",
            DescriptionsWithValue(
              Some("1"),
              List("<empty>", "value of type int", "Example: 8088", "Database related")
            )
          ),
          Path(
            "URL",
            DescriptionsWithValue(
              Some("value"),
              List("<empty>", "value of type string", "optional value", "Example: abc.com", "Database related")
            )
          )
        )
      )
  )
}
