package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.config.ConfigDocs, ConfigDocs._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ?? "Example: 8088" |@|
      string("URL").optional ?? "Example: abc.com")(Database.apply, Database.unapply) ?? "Database related"

  assert(
    generateDocs(config) ==
      ConfigDocs.Zip(
        ConfigDocs.Nested(
          "PORT",
          Leaf((Set.empty), List("value of type int", "Example: 8088", "Database related"))
        ),
        ConfigDocs.Nested(
          "URL",
          Leaf(
            (Set.empty),
            List("value of type string", "optional value", "Example: abc.com", "Database related")
          )
        )
      )
  )

  assert(
    generateReport(config, Database(1, Some("value"))) ==
      Right(
        ConfigDocs.Zip(
          ConfigDocs.Nested(
            "PORT",
            Leaf(
              (Set.empty),
              List("value of type int", "Example: 8088", "Database related"),
              Some("1")
            )
          ),
          ConfigDocs.Nested(
            "URL",
            Leaf(
              (Set.empty),
              List("value of type string", "optional value", "Example: abc.com", "Database related"),
              Some("value")
            )
          )
        )
      )
  )
}
