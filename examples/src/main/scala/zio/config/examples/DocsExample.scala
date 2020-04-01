package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ConfigDocs.Details._
import zio.config.ConfigDocs.{ Path, _ }
import zio.config._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ?? "Example: 8088" |@|
      string("URL").optional ?? "Example: abc.com")(Database.apply, Database.unapply) ?? "Database related"

  assert(
    generateDocs(config) ==
      Both(
        Path(
          "PORT",
          Descriptions(Sources(Nil), List("value of type int", "Example: 8088", "Database related"))
        ),
        Path(
          "URL",
          Descriptions(
            Sources(Nil),
            List("value of type string", "optional value", "Example: abc.com", "Database related")
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
              Sources(Nil),
              List("value of type int", "Example: 8088", "Database related")
            )
          ),
          Path(
            "URL",
            DescriptionsWithValue(
              Some("value"),
              Sources(Nil),
              List("value of type string", "optional value", "Example: abc.com", "Database related")
            )
          )
        )
      )
  )
}
