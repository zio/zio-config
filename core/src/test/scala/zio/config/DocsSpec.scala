package zio.config

import java.io.{ File, PrintWriter }
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

          val caseClass3 =
            (string("token_id").from(source1.orElse(source2)) |@| string(
              "username"
            ))(TestCase1.CaseClass3.apply, TestCase1.CaseClass3.unapply)

          /*
          val caseClass4 =
            (string("x").from(source1.orElse(source2)) |@| string(
              "y"
            ))(TestCase1.CaseClass4.apply, TestCase1.CaseClass4.unapply)

          val caseClass5 =
            (string("x").from(source1.orElse(source2)) |@| string(
              "y"
            ))(TestCase1.CaseClass5.apply, TestCase1.CaseClass5.unapply)
           */

          val either1 =
            (nested("aws1")(caseClass2))
              .orElseEither(nested("credentials")(caseClass3))

          /*          val either2 =
            (nested("aws2")(caseClass4))
              .orElseEither(nested("credentials2")(caseClass5))*/

          /*
          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| either1 |@| either2 |@| list(
              "locations"
            )(string))(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )
           */

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| either1)(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val result = generateDocs(config from ConfigSource.fromMap(Map.empty, source = "system environment"))

          val writer = new PrintWriter(new File("config.md"))

          writer.write(result.toTable.asMarkdownContent)
          writer.close()

          assert(Some(result))(
            equalTo(None: Option[ConfigDocs])
          )
        }
      )
    )

object DocsSpecUtils {
  object TestCase1 {
    case class CaseClass1(
      a: String,
      b: Either[CaseClass2, CaseClass3]
    )
    case class CaseClass2(a: String, b: String)
    case class CaseClass3(c: String, d: String)
    case class CaseClass4(c: String, d: String)
    case class CaseClass5(c: String, d: String)
  }
}
