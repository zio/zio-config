package zio.config

import java.io.{ File, PrintWriter }

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

          val caseClass3 =
            (string("token_id") |@| string("username"))(TestCase1.CaseClass3.apply, TestCase1.CaseClass3.unapply)

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| (nested("aws")(caseClass2)).orElseEither(nested("credentials")(caseClass3)) |@| list(
              "locations"
            )(string))(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val result = generateDocs(config from ConfigSource.fromMap(Map.empty, source = "system environment"))

          val writer = new PrintWriter(new File("config.md"))

          writer.write(result.toTable(None).asMarkdownContent)
          writer.close()

          assert(Some(result))(
            equalTo(None: Option[ConfigDocs])
          )
        }
      )
    )

object DocsSpecUtils {
  object TestCase1 {
    case class CaseClass1(a: String, b: Either[CaseClass2, CaseClass3], c: List[String])
    case class CaseClass2(a: String, b: String)
    case class CaseClass3(c: String, d: String)
  }
}
