package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test._

object GenerateDocsTest
    extends BaseSpec(
      suite("Generate docs")(
        test("generate docs") {
          val expected =
            s"""
               |## Configuration Details
               |
               |
               ||FieldName|Format                     |Description|Sources|
               ||---      |---                        |---        |---    |
               ||         |[all-of](fielddescriptions)|           |       |
               |
               |### Field Descriptions
               |
               ||FieldName                 |Format               |Description                                             |Sources|
               ||---                       |---                  |---                                                     |---    |
               ||SECRET                    |primitive            |value of type string, optional value, Application secret|       |
               ||[CREDENTIALS](credentials)|[all-of](credentials)|Credentials                                             |       |
               ||[DATABASE](database)      |[all-of](database)   |Database                                                |       |
               |
               |### CREDENTIALS
               |
               ||FieldName|Format   |Description                           |Sources|
               ||---      |---      |---                                   |---    |
               ||USERNAME |primitive|value of type string, Example: ZioUser|       |
               ||PASSWORD |primitive|value of type string, Example: ZioPass|       |
               |
               |### DATABASE
               |
               ||FieldName|Format   |Description                           |Sources|
               ||---      |---      |---                                   |---    |
               ||PORT     |primitive|value of type int, Example: 8088      |       |
               ||URL      |primitive|value of type string, Example: abc.com|       |
               |""".stripMargin

          assert(generateDocs(GenerateDocsTestUtils.descriptor).toTable.asGithubFlavouredMarkdown)(equalTo(expected))
        }
      )
    )

object GenerateDocsTestUtils {
  final case class Credentials(user: String, password: String)
  final case class Database(port: Int, url: String)
  final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

  def descriptor: ConfigDescriptor[AppConfig] = {
    val credentials = (string("USERNAME") ?? "Example: ZioUser" |@| string("PASSWORD") ?? "Example: ZioPass")(
      Credentials.apply,
      Credentials.unapply
    ) ?? "Credentials"

    val database = (int("PORT") ?? "Example: 8088" |@| string("URL") ?? "Example: abc.com")(
      Database.apply,
      Database.unapply
    ) ?? "Database"

    (string("SECRET").optional ?? "Application secret" |@| nested("CREDENTIALS")(credentials) |@| nested(
      "DATABASE"
    )(database))(
      AppConfig.apply,
      AppConfig.unapply
    )
  }
}
