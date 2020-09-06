package zio.config

import java.util.Properties

import ConfigDescriptor._
import zio.config.DocsSpecUtils.TestCase1.{ CaseClass5, CaseClass6 }
import zio.config.DocsSpecUtils._
import zio.test.Assertion._
import zio.test._

object DocsSpec
    extends BaseSpec(
      suite("partial products fail instead of returning none")(
        test(
          "Generate docs for simple config"
        ) {

          val caseClass2 =
            (string("region") |@| string("account_name"))(TestCase1.CaseClass2.apply, TestCase1.CaseClass2.unapply)

          val source1 =
            ConfigSource.fromProperties(new Properties(), "docker env")

          val source2 =
            ConfigSource.fromProperties(new Properties(), "system properties")

          val tokenAndUsername =
            (string("token_id").from(source1.orElse(source2)) |@| string("username"))(
              TestCase1.CaseClass3.apply,
              TestCase1.CaseClass3.unapply
            )

          val azureConfig =
            nested("azure") {
              (string("a") |@| string("b"))(TestCase1.CaseClass4.apply, TestCase1.CaseClass4.unapply)
            }

          val awsConfig =
            (nested("aws")(caseClass2))
              .orElseEither(nested("credentials")(tokenAndUsername))

          val caseClass5Config =
            nested("db1")((string("f") |@| string("g"))(CaseClass5.apply, CaseClass5.unapply))

          val caseClass6Config =
            nested("db2")((string("h") |@| string("i"))(CaseClass6.apply, CaseClass6.unapply))

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| awsConfig |@| azureConfig |@| caseClass5Config.optional |@| caseClass6Config
              .default(CaseClass6("", "")))(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val finalSource =
            ConfigSource.fromMap(Map.empty, source = "system environment")

          val result: String =
            generateDocs(config from finalSource).toTable.asGithubFlavouredMarkdown

          assert(result)(
            equalTo(DocsSpecUtils.expected)
          )
        }
      )
    )

object DocsSpecUtils {
  object TestCase1 {
    case class CaseClass1(
      a: String,
      b: Either[CaseClass2, CaseClass3],
      c: CaseClass4,
      d: Option[CaseClass5],
      e: CaseClass6
    )
    case class CaseClass2(a: String, b: String)
    case class CaseClass3(c: String, d: String)
    case class CaseClass4(c: String, d: String)
    case class CaseClass5(e: String, f: String)
    case class CaseClass6(f: String, g: String)
  }

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
       ||FieldName     |Format                           |Description         |Sources           |
       ||---           |---                              |---                 |---               |
       ||user          |primitive                        |value of type string|system environment|
       ||              |[any-one-of](fielddescriptions-1)|                    |                  |
       ||[azure](azure)|[all-of](azure)                  |                    |                  |
       ||[db1](db1)    |[all-of](db1)                    |                    |                  |
       ||[db2](db2)    |[all-of](db2)                    |                    |                  |
       |
       |### Field Descriptions
       |
       ||FieldName                 |Format               |Description|Sources|
       ||---                       |---                  |---        |---    |
       ||[aws](aws)                |[all-of](aws)        |           |       |
       ||[credentials](credentials)|[all-of](credentials)|           |       |
       |
       |### aws
       |
       ||FieldName   |Format   |Description         |Sources           |
       ||---         |---      |---                 |---               |
       ||region      |primitive|value of type string|system environment|
       ||account_name|primitive|value of type string|system environment|
       |
       |### credentials
       |
       ||FieldName|Format   |Description         |Sources                                          |
       ||---      |---      |---                 |---                                              |
       ||token_id |primitive|value of type string|docker env, system properties, system environment|
       ||username |primitive|value of type string|system environment                               |
       |
       |### azure
       |
       ||FieldName|Format   |Description         |Sources           |
       ||---      |---      |---                 |---               |
       ||a        |primitive|value of type string|system environment|
       ||b        |primitive|value of type string|system environment|
       |
       |### db1
       |
       ||FieldName|Format   |Description                         |Sources           |
       ||---      |---      |---                                 |---               |
       ||f        |primitive|value of type string, optional value|system environment|
       ||g        |primitive|value of type string, optional value|system environment|
       |
       |### db2
       |
       ||FieldName|Format   |Description                                       |Sources           |
       ||---      |---      |---                                               |---               |
       ||h        |primitive|value of type string, default value: CaseClass6(,)|system environment|
       ||i        |primitive|value of type string, default value: CaseClass6(,)|system environment|
       |""".stripMargin
}
