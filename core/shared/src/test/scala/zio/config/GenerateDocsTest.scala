package zio.config

import zio.config.ConfigDescriptor._
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object GenerateDocsTest extends BaseSpec {

  val spec: Spec[Environment, Any] =
    suite("Generate docs")(
      test("optional nested") {
        val inner = (int("a") zip int("b"))
        val outer = nested("c")(inner).optional

        val doc   = generateDocs(outer)
        val table = doc.toTable

        assert(table)(
          equalTo(
            Table(
              List(
                Table.TableRow(
                  List(Table.FieldName.Key("c")),
                  Some(Table.Format.AllOf),
                  List(ConfigDocs.Description(None, "optional value")),
                  Some(
                    Table(
                      List(
                        Table.TableRow(
                          List(Table.FieldName.Key("a")),
                          Some(Table.Format.Primitive),
                          List(ConfigDocs.Description(Some("a"), "value of type int")),
                          None,
                          Set.empty
                        ),
                        Table.TableRow(
                          List(Table.FieldName.Key("b")),
                          Some(Table.Format.Primitive),
                          List(ConfigDocs.Description(Some("b"), "value of type int")),
                          None,
                          Set.empty
                        )
                      )
                    )
                  ),
                  Set.empty
                )
              )
            )
          )
        )
      },
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

        assert(generateDocs(GenerateDocsTestUtils.descriptor).toTable.toGithubFlavouredMarkdown)(equalTo(expected))
      } @@ ignore
    )
}

object GenerateDocsTestUtils {
  final case class Credentials(user: String, password: String)
  final case class Database(port: Int, url: String)
  final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

  def descriptor: ConfigDescriptor[AppConfig] = {
    val credentials = (string("USERNAME") ?? "Example: ZioUser" zip string("PASSWORD") ?? "Example: ZioPass")
      .to[Credentials] ?? "Credentials"

    val database = (int("PORT") ?? "Example: 8088" zip string("URL") ?? "Example: abc.com").to[Database] ?? "Database"

    (string("SECRET").optional ?? "Application secret" zip nested("CREDENTIALS")(credentials) zip nested(
      "DATABASE"
    )(database)).to[AppConfig]
  }
}
