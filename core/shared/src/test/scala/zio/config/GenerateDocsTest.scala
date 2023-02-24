package zio.config

import zio.Config
import zio.test.Assertion._
import zio.test._

import Config._

object GenerateDocsTest extends BaseSpec {

  val spec: Spec[Environment, Any] =
    suite("Generate docs")(
      test("generate docs") {
        val expected =
          s"""|## Configuration Details
              |
              |
              ||FieldName|Format                     |Description|Sources|
              ||---      |---                        |---        |---    |
              ||         |[all-of](fielddescriptions)|           |       |
              |
              |### Field Descriptions
              |
              ||FieldName                 |Format               |Description                        |Sources|
              ||---                       |---                  |---                                |---    |
              ||SECRET                    |primitive            |a text property, Application secret|       |
              ||[CREDENTIALS](credentials)|[all-of](credentials)|Credentials                        |       |
              ||[DATABASE](database)      |[all-of](database)   |Database                           |       |
              |
              |### CREDENTIALS
              |
              ||FieldName|Format   |Description                      |Sources|
              ||---      |---      |---                              |---    |
              ||USERNAME |primitive|a text property, Example: ZioUser|       |
              ||PASSWORD |primitive|a text property, Example: ZioPass|       |
              |
              |### DATABASE
              |
              ||FieldName|Format   |Description                       |Sources|
              ||---      |---      |---                               |---    |
              ||PORT     |primitive|an integer property, Example: 8088|       |
              ||URL      |primitive|a text property, Example: abc.com |       |""".stripMargin

        assert(generateDocs(GenerateDocsTestUtils.descriptor).toTable.toGithubFlavouredMarkdown.trim)(
          equalTo(expected.trim)
        )
      }
    )
}

object GenerateDocsTestUtils {
  final case class Credentials(user: String, password: String)
  final case class Database(port: Int, url: String)
  final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

  def descriptor: Config[AppConfig] = {
    val credentials: Config[Credentials] =
      (string("USERNAME") ?? "Example: ZioUser" zip string("PASSWORD") ?? "Example: ZioPass")
        .to[Credentials] ?? "Credentials"

    val database = (int("PORT") ?? "Example: 8088" zip string("URL") ?? "Example: abc.com").to[Database] ?? "Database"

    string("SECRET").optional
      .??("Application secret")
      .zip(
        credentials.nested("CREDENTIALS")
      )
      .zip(database.nested("DATABASE"))
      .to[AppConfig]
  }
}
