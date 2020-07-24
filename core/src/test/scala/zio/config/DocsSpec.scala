package zio.config

import java.util.Properties

import ConfigDescriptor._
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

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| awsConfig |@| azureConfig)(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val finalSource =
            ConfigSource.fromMap(Map.empty, source = "system environment")

          val result: String =
            generateDocs(config from finalSource).toTable.asMarkdownContent

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
      c: CaseClass4
    )
    case class CaseClass2(a: String, b: String)
    case class CaseClass3(c: String, d: String)
    case class CaseClass4(c: String, d: String)
  }

  val expected =
    s"""
       |## Configuration Details
       |
       |
       ||FieldName             |Format                 |Description         |Sources           |
       ||---                   |---                    |---                 |---               |
       ||[azure](#root.azure_2)|[all-of](#root.azure_2)|                    |                  |
       ||                      |[any-one-of](#root_1)  |                    |                  |
       ||user                  |primitive              |value of type string|system environment|
       |
       |### root.azure_2
       |
       ||FieldName|Format   |Description         |Sources           |
       ||---      |---      |---                 |---               |
       ||b        |primitive|value of type string|system environment|
       ||a        |primitive|value of type string|system environment|
       |
       |### root_1
       |
       ||FieldName                           |Format                         |Description|Sources|
       ||---                                 |---                            |---        |---    |
       ||[credentials](#root.credentials_1_1)|[all-of](#root.credentials_1_1)|           |       |
       ||[aws](#root.aws_1_0)                |[all-of](#root.aws_1_0)        |           |       |
       |
       |### root.credentials_1_1
       |
       ||FieldName|Format   |Description         |Sources                                          |
       ||---      |---      |---                 |---                                              |
       ||username |primitive|value of type string|system environment                               |
       ||token_id |primitive|value of type string|docker env, system properties, system environment|
       |
       |### root.aws_1_0
       |
       ||FieldName   |Format   |Description         |Sources           |
       ||---         |---      |---                 |---               |
       ||account_name|primitive|value of type string|system environment|
       ||region      |primitive|value of type string|system environment|
       |""".stripMargin

}
