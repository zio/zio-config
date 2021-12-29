package zio.config.examples.typesafe

import zio.IO
import zio.config._

import typesafe._
import magnolia._
import examples._
import ConfigDescriptor._

object TypesafeConfigSimple extends App with EitherImpureOps {
  // A nested example with type safe config, and usage of magnolia
  final case class Details(name: String, age: Int)

  final case class Account(accountId: Option[Either[Int, String]], regions: List[String], details: Option[Details])

  final case class Database(port: Option[Int], url: String)

  final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon =
    """
    accounts = [
      {
          accountId: jon
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
         accountId: bb
         regions : [us-some, ff, gg]


      }
    ]

    users = [1, 2, 3]
    database {
        port : 100
        url  : postgres
    }


    """

  val details: ConfigDescriptor[Details] = (string("name") zip int("age")).to[Details]

  val accountConfig: ConfigDescriptor[Account] =
    (int("accountId").orElseEither(string("accountId")).optional zip list(
      "regions"
    )(string) zip nested("details")(details).optional).to[Account]

  val databaseConfig: ConfigDescriptor[Database] =
    (int("port").optional zip string("url")).to[Database]

  val awsDetailsConfig: ConfigDescriptor[AwsDetails] =
    (nested("accounts")(list(accountConfig)) zip nested("database")(
      databaseConfig
    ) zip list("users")(int)).to[AwsDetails]

  val listResult: IO[ReadError[String], AwsDetails] =
    read(awsDetailsConfig from ConfigSource.fromHoconString(validHocon))

  assert(
    listResult equalM
      AwsDetails(
        List(
          Account(
            Some(Right("jon")),
            List("us-east", "dd", "ee"),
            Some(Details("jaku", 10))
          ),
          Account(
            Some(Left(123)),
            List("us-west", "ab", "cd"),
            Some(Details("zak", 11))
          ),
          Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )

  val automaticAwsDetailsConfig: ConfigDescriptor[AwsDetails] = descriptor[AwsDetails]

  val automaticResult: IO[ReadError[String], AwsDetails] =
    read(automaticAwsDetailsConfig from ConfigSource.fromHoconString(validHocon))

  assert(
    automaticResult equalM
      AwsDetails(
        List(
          Account(
            Some(Right("jon")),
            List("us-east", "dd", "ee"),
            Some(Details("jaku", 10))
          ),
          Account(
            Some(Left(123)),
            List("us-west", "ab", "cd"),
            Some(Details("zak", 11))
          ),
          Account(Some(Right("bb")), List("us-some", "ff", "gg"), None)
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
          accountId: jon
      }
      {
          region : us-west
          accountId: chris
      }
      {

      }
    ]

    users = [1, 2, 3]

    database {
        port : 1abcd
        url  : postgres
    }

    """

  println(
    read(descriptor[AwsDetails] from ConfigSource.fromHoconString(invalidHocon)).either.unsafeRun
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
