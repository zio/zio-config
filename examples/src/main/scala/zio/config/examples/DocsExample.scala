package zio.config.examples

import zio.config._
import Config._
import zio.config.actions.ConfigDocs
import zio.config.actions.ConfigDocs.KeyDescription

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ? "Example: 8088" |@|
      string("URL").optional ? "Example: abc.com")(Database.apply, Database.unapply) ? "Database related"

  assert(
    docs(config, Some(Database(1, Some("value")))) == ConfigDocs(
      List(
        KeyDescription("PORT", Some("1"), List("value of type int", "Example: 8088", "Database related")),
        KeyDescription(
          "URL",
          Some("value"),
          List("value of type string", "optional value", "Example: abc.com", "Database related")
        )
      ),
      None
    )
  )

}
