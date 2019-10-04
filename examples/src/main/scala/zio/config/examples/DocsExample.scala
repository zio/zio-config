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
    docs(config) == ConfigDocs(
      List(
        KeyDescription("PORT", List("value of type int", "Example: 8088", "Database related")),
        KeyDescription("URL", List("value of type string", "Optional value", "Example: abc.com", "Database related"))
      ),
      None
    )
  )

}
