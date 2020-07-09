package zio.config.examples.typesafe

import zio.config._, ConfigDescriptor._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfigSource.fromHoconString

object TypesafeConfigSimpleExample extends App with EitherImpureOps {
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

  val details = (string("name") |@| int("age"))(Details.apply, Details.unapply)

  val accountConfig =
    (int("accountId").orElseEither(string("accountId")).optional |@| list(
      "regions"
    )(string) |@| nested("details")(details).optional)(
      Account.apply,
      Account.unapply
    )

  val databaseConfig =
    (int("port").optional |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(
      databaseConfig
    ) |@| list("users")(int))(AwsDetails.apply, AwsDetails.unapply)

  val listResult =
    fromHoconString(validHocon) match {
      case Left(value)   => Left(value)
      case Right(source) => read(awsDetailsConfig from source)
    }

  assert(
    listResult ==
      Right(
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
  )
  val automaticAwsDetailsConfig = descriptor[AwsDetails]

  val automaticResult =
    fromHoconString(validHocon) match {
      case Left(value)   => Left(value)
      case Right(source) => read(automaticAwsDetailsConfig from source)
    }

  assert(
    automaticResult ==
      Right(
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
    read(descriptor[AwsDetails] from fromHoconString(invalidHocon).loadOrThrow)
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
