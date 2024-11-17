package zio.config.examples.typesafe

import zio.config._
import zio.config.examples.ZioOps
import zio.{Config, ConfigProvider, IO}

import typesafe._
import magnolia._
import Config._

object TypesafeConfigSimple extends App with EitherImpureOps {
  // A nested example with type safe config, and usage of magnolia
  final case class Details(name: String, age: Int)

  final case class Account(accountId: Option[Int], regions: List[String], details: Option[Details])

  final case class Database(port: Option[Int], url: String)

  final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon =
    """
    accounts = [
      {
          accountId: 123
          regions : [us-east, dd, ee]
          details {
            name : jaku
            age  : 10
          }
      }
      {
          accountId: 123
          regions : [us-west,  ab, cd]
          details {
            name : zak
            age: 11
          }
      }
      {
         accountId: 456
         regions : [us-some, ff, gg]


      }
    ]

    users = [1, 2, 3]
    database {
        port : 100
        url  : postgres
    }


    """

  val details: Config[Details] = (string("name") zip int("age")).to[Details]

  val accountConfig: Config[Account] =
    (int("accountId").optional zip listOf("regions", string) zip (details.nested("details")).optional)
      .to[Account]

  val databaseConfig: Config[Database] =
    (int("port").optional zip string("url")).to[Database]

  val awsDetailsConfig: Config[AwsDetails] =
    (listOf(accountConfig).nested("accounts") zip (
      databaseConfig.nested("database")
    ) zip listOf("users", int)).to[AwsDetails]

  val listResult: IO[Config.Error, AwsDetails] =
    read(awsDetailsConfig from ConfigProvider.fromHoconString(validHocon))

  assert(
    listResult equalM
      AwsDetails(
        List(
          Account(
            Some(123),
            List("us-east", "dd", "ee"),
            Some(Details("jaku", 10))
          ),
          Account(
            Some(123),
            List("us-west", "ab", "cd"),
            Some(Details("zak", 11))
          ),
          Account(Some(456), List("us-some", "ff", "gg"), None)
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )

  val automaticAwsDetailsConfig: Config[AwsDetails] = deriveConfig[AwsDetails]

  val automaticResult: IO[Config.Error, AwsDetails] =
    read(automaticAwsDetailsConfig from ConfigProvider.fromHoconString(validHocon))

  assert(
    listResult equalM
      AwsDetails(
        List(
          Account(
            Some(123),
            List("us-east", "dd", "ee"),
            Some(Details("jaku", 10))
          ),
          Account(
            Some(123),
            List("us-west", "ab", "cd"),
            Some(Details("zak", 11))
          ),
          Account(Some(456), List("us-some", "ff", "gg"), None)
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )

  // If defining such a configuration description is tedious, you may rely on (experimental) magnolia module

  val invalidHocon =
    """
    accounts = [
      {
          accountId: 122
      }
      {
          region : us-west
          accountId: 221
      }
      {

      }
    ]

    users = [1, 2, 3]

    database {
        port : 1111
        url  : postgres
    }

    """

  println(
    read(deriveConfig[AwsDetails] from ConfigProvider.fromHoconString(invalidHocon)).either
      .map(_.swap.map(_.prettyPrint()).swap)
      .unsafeRun
  )
  /*
    ╥
    ╠══╦══╦══╦══╗
    ║  ║  ║  ║  ║
    ║  ║  ║  ║  ╠─FormatError
    ║  ║  ║  ║  ║ cause: Provided value is 1abcd, expecting the type int
    ║  ║  ║  ║  ║ path: database.port
    ║  ║  ║  ║  ▼
    ║  ║  ║  ║
    ║  ║  ║  ╠─MissingValue
    ║  ║  ║  ║ path: accounts[0].regions
    ║  ║  ║  ▼
    ║  ║  ║
    ║  ║  ╠─MissingValue
    ║  ║  ║ path: accounts[1].regions
    ║  ║  ▼
    ║  ║
    ║  ╠─MissingValue
    ║  ║ path: accounts[2].regions
    ║  ▼
    ▼
   */
}
