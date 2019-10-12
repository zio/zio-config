package zio.config.examples

import zio.config._
import Config._, zio.config.actions.ConfigDocs._
import zio.config.actions.ConfigDocs.PathDetails

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ? "Example: 8088" |@|
      string("URL").optional ? "Example: abc.com")(Database.apply, Database.unapply) ? "Database related"

  assert(
    docs(config, Some(Database(1, Some("value")))) ==
      And(
        PathDetails(Vector("PORT"), Some("1"), List("value of type int", "Example: 8088", "Database related")),
        PathDetails(
          Vector("URL"),
          Some("value"),
          List("value of type string", "optional value", "Example: abc.com", "Database related")
        )
      )
  )
}
