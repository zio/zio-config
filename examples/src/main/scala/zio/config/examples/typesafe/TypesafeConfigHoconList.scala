package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ int, list, nested, string }
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
import zio.config._

object TypesafeConfigHoconList extends App {
  val runtime = new DefaultRuntime {}

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
          details {
            name : meen
            age: 12
          }

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
    (int("accountId").orElseEither(string("accountId")).optional |@| list(string("regions")) |@| nested("details")(
      details
    ).optional)(
      Account.apply,
      Account.unapply
    )

  val databaseConfig = (int("port").optional |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig) |@| list(int("users")))(
      AwsDetails.apply,
      AwsDetails.unapply
    )

  val listResult =
    read(awsDetailsConfig from hocon(Right(validHocon)))

  println(runtime.unsafeRun(listResult))

  assert(
    runtime.unsafeRun(listResult) ==
      AwsDetails(
        List(
          Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
          Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
          Account(Some(Right("bb")), List("us-some", "ff", "gg"), Some(Details("meen", 12)))
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )
  val automaticAwsDetailsConfig = description[AwsDetails]

  val automaticResult = runtime.unsafeRun(read(automaticAwsDetailsConfig from hocon(Right(validHocon))))

  assert(
    automaticResult ==
      AwsDetails(
        List(
          Account(Some(Right("jon")), List("us-east", "dd", "ee"), Some(Details("jaku", 10))),
          Account(Some(Left(123)), List("us-west", "ab", "cd"), Some(Details("zak", 11))),
          Account(Some(Right("bb")), List("us-some", "ff", "gg"), Some(Details("meen", 12)))
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
          accountId: null
      }
    ]

    users = [1, 2, 3]

    database {
        port : 1abcd
        url  : postgres
    }

    """

  println(runtime.unsafeRun(read(description[AwsDetails] from hocon(Right(invalidHocon))).either))

}
