package zio.config

import zio.{Config, ConfigProvider}, Config._
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object GenerateDocsTest extends BaseSpec {

  val spec: Spec[Environment, Any] =
    suite("Generate docs")(
      test("optional nested") {
        val inner = (int("a") zip int("b"))
        val outer = inner.nested("c").optional

        val doc   = generateDocs(outer)
        val table = doc.toTable

        assert(table)(
          equalTo(
            Table(
              List(
                Table.TableRow(
                  List(Table.FieldName.Key("c")),
                  Some(Table.Format.AllOf),
                  Nil,
                  Some(
                    Table(
                      List(
                        Table.TableRow(
                          List(Table.FieldName.Key("a")),
                          Some(Table.Format.Primitive),
                          Nil,
                          None,
                          Set.empty
                        ),
                        Table.TableRow(
                          List(Table.FieldName.Key("b")),
                          Some(Table.Format.Primitive),
                          Nil,
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
          s"""|## Configuration Details
              |
              |
              ||FieldName|Format                     |Description|Sources|
              ||---      |---                        |---        |---    |
              ||         |[all-of](fielddescriptions)|           |       |
              |
              |### Field Descriptions
              |
              ||FieldName                 |Format               |Description       |Sources|
              ||---                       |---                  |---               |---    |
              ||SECRET                    |primitive            |Application secret|       |
              ||[CREDENTIALS](credentials)|[all-of](credentials)|Credentials       |       |
              ||[DATABASE](database)      |[all-of](database)   |Database          |       |
              |
              |### CREDENTIALS
              |
              ||FieldName|Format   |Description     |Sources|
              ||---      |---      |---             |---    |
              ||USERNAME |primitive|Example: ZioUser|       |
              ||PASSWORD |primitive|Example: ZioPass|       |
              |
              |### DATABASE
              |
              ||FieldName|Format   |Description     |Sources|
              ||---      |---      |---             |---    |
              ||PORT     |primitive|Example: 8088   |       |
              ||URL      |primitive|Example: abc.com|       |""".stripMargin

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
