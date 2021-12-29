package zio.config.typesafe

import zio.config._
import zio.config.typesafe.TypesafeConfigSource.fromHoconString
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}

import magnolia._
import ConfigDescriptor._

object TypesafeConfigSimpleSpec extends DefaultRunnableSpec {
  final case class Details(name: String, age: Int)

  final case class Account(accountId: Option[Either[Int, String]], regions: List[String], details: Option[Details])

  final case class Database(port: Option[Int], url: String)

  final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon: String =
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

  val spec: ZSpec[Environment, Failure] = suite("TypesafeConfig")(
    testM("A nested example with typesafe HOCON config") {

      val details       = (string("name") zip int("age")).to[Details]
      val accountConfig =
        (int("accountId").orElseEither(string("accountId")).optional zip list("regions")(string) zip nested("details")(
          details
        ).optional).to[Account]

      val databaseConfig   = (int("port").optional zip string("url")).to[Database]
      val awsDetailsConfig =
        (nested("accounts")(list(accountConfig)) zip nested("database")(databaseConfig) zip list("users")(int))
          .to[AwsDetails]

      val listResult =
        read(awsDetailsConfig from fromHoconString(validHocon))

      val expectedResult =
        AwsDetails(
          List(
            Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
            Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
            Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
          ),
          Database(Some(100), "postgres"),
          List(1, 2, 3)
        )

      assertM(listResult)(equalTo(expectedResult))
    },
    testM("A nested example with typesafe HOCON config and Magnlia") {
      val automaticAwsDetailsConfig = descriptor[AwsDetails]

      val automaticResult =
        read(automaticAwsDetailsConfig from fromHoconString(validHocon))

      val expectedResult =
        AwsDetails(
          List(
            Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
            Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
            Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
          ),
          Database(Some(100), "postgres"),
          List(1, 2, 3)
        )

      assertM(automaticResult)(equalTo(expectedResult))
    }
  )
}
