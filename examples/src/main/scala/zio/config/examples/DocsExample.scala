package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config.ConfigDocs._
import zio.config._

object DocsExample extends App {

  final case class Database(port: Int, url: Option[String])

  val config =
    (int("PORT") ?? "Example: 8088" |@|
      string("URL").optional ?? "Example: abc.com")(Database.apply, Database.unapply) ?? "Database related"

  assert(
    generateDocs(config) ==
      Both(
        NestedPath(
          "PORT",
          Leaf(Sources(Set.empty), List("value of type int", "Example: 8088", "Database related"))
        ),
        NestedPath(
          "URL",
          Leaf(
            Sources(Set.empty),
            List("value of type string", "optional value", "Example: abc.com", "Database related")
          )
        )
      )
  )

  assert(
    generateDocsWithValue(config, Database(1, Some("value"))) ==
      Right(
        Both(
          NestedPath(
            "PORT",
            Leaf(
              Sources(Set.empty),
              List("value of type int", "Example: 8088", "Database related"),
              Some("1")
            )
          ),
          NestedPath(
            "URL",
            Leaf(
              Sources(Set.empty),
              List("value of type string", "optional value", "Example: abc.com", "Database related"),
              Some("value")
            )
          )
        )
      )
  )
}
