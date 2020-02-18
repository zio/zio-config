package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ int, list, nested, string }
import zio.config.ReadError.{ AndErrors, MissingValue, ParseError }
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon
import zio.config._

object TypesafeConfigHoconList extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: List[String], accountId: Option[Either[Int, String]])
  final case class Database(port: Option[Int], url: String)
  final case class AwsDetails(accounts: List[Account], database: Database, users: List[Int])

  val validHocon =
    """
    accounts = [
      {
          region : [us-east, dd, ee]
          accountId: jon
      }
      {
          region : [us-west,  ab, cd]
          accountId: 123
      }
      {
          region : [us-some, ff, gg]
          accountId: null
      }
    ]

    users = [1, 2, 3]

    database {
        port : 100
        url  : postgres
    }

    """

  val accountConfig =
    (list(string("region")) |@| int("accountId").orElseEither(string("accountId")).optional)(
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

  assert(
    runtime.unsafeRun(listResult) ==
      AwsDetails(
        List(
          Account(List("us-east", "dd", "ee"), Some(Right("jon"))),
          Account(List("us-west", "ab", "cd"), Some(Left(123))),
          Account(List("us-some", "ff", "gg"), None)
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )

  // If defining such a configuration description is tedious, you may rely on (experimental) magnolia module
  val automaticAwsDetailsConfig = description[AwsDetails]

  assert(
    runtime.unsafeRun(read(automaticAwsDetailsConfig from hocon(Right(validHocon)))) ==
      AwsDetails(
        List(
          Account(List("us-east", "dd", "ee"), Some(Right("jon"))),
          Account(List("us-west", "ab", "cd"), Some(Left(123))),
          Account(List("us-some", "ff", "gg"), None)
        ),
        Database(Some(100), "postgres"),
        List(1, 2, 3)
      )
  )

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

  // AndErrors essentially tells us that, fix all of it ! Needn't be concerned about the nestedness
  assert(
    runtime.unsafeRun(read(description[AwsDetails] from hocon(Right(invalidHocon))).either) ==
      Left(
        List(
          AndErrors[Vector[String]](
            ::(
              ParseError(Vector("database", "port"), ReadFunctions.parseErrorMessage("1abcd", "int")),
              singleton(
                AndErrors(
                  singleton(
                    AndErrors(
                      ::(
                        MissingValue(Vector("accounts", "region"), Some(0)): ReadError[Vector[String]],
                        (MissingValue(Vector("accounts", "region"), Some(2)): ReadError[Vector[String]]) :: Nil
                      )
                    ): ReadError[Vector[String]]
                  )
                )
              )
            )
          )
        )
      )
  )
}
