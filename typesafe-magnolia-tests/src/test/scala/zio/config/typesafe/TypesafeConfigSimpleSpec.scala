package zio.config.typesafe

import zio.config.ConfigDescriptor.{ int, list, nested, string }
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.read
import zio.config.typesafe.TypesafeConfigSource.fromHoconString
import zio.test.{ suite, test, DefaultRunnableSpec }
import zio.test.Assertion._
import zio.test._

object TypesafeConfigSimpleSpec extends DefaultRunnableSpec {
  final case class Details(name: String, age: Int)

  final case class Account(accountId: Option[Either[Int, String]], regions: List[String], details: Option[Details])

  final case class Database(port: Option[Int], url: String)

  final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon =
    """
      |accounts = [
      |  {
      |    accountId: jon
      |    regions: [us-east, dd, ee]
      |    details {
      |      name: jaku
      |      age: 10
      |    }
      |  }
      |  {
      |    accountId: 123
      |    regions: [us-west, ab, cd]
      |    details {
      |      name: zak
      |      age: 11
      |    }
      |  }
      |  {
      |    accountId: bb
      |    regions: [us-some, ff, gg]
      |  }
      |]
      |
      |users = [1, 2, 3]
      |database {
      |  port: 100
      |  url: postgres
      |}""".stripMargin

  val spec = suite("TypesafeConfig")(
    test("A nested example with typesafe HOCON config") {

      val details = (string("name") |@| int("age"))(Details.apply, Details.unapply)
      val accountConfig =
        (int("accountId").orElseEither(string("accountId")).optional |@| list("regions")(string) |@| nested("details")(
          details
        ).optional)(
          Account.apply,
          Account.unapply
        )
      val databaseConfig = (int("port").optional |@| string("url"))(Database.apply, Database.unapply)
      val awsDetailsConfig =
        (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig) |@| list("users")(int))(
          AwsDetails.apply,
          AwsDetails.unapply
        )
      val listResult =
        fromHoconString(validHocon) match {
          case Left(value)   => Left(value)
          case Right(source) => read(awsDetailsConfig from source)
        }

      val expectedResult = Right(
        AwsDetails(
          List(
            Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
            Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
            Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
          ),
          Database(Some(100), "postgres"),
          List(1, 2, 3)
        )
      )

      assert(listResult)(equalTo(expectedResult))
    },
    test("A nested example with typesafe HOCON config and Magnlia") {
      val automaticAwsDetailsConfig = descriptor[AwsDetails]

      val automaticResult =
        fromHoconString(validHocon) match {
          case Left(value)   => Left(value)
          case Right(source) => read(automaticAwsDetailsConfig from source)
        }

      val expectedResult = Right(
        AwsDetails(
          List(
            Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
            Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
            Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
          ),
          Database(Some(100), "postgres"),
          List(1, 2, 3)
        )
      )

      assert(automaticResult)(equalTo(expectedResult))
    }
  )
}
