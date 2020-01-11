package zio.config.examples

import zio.config.typesafe.TypeSafeConfigSource._
import zio.config._, ConfigDescriptor._
import zio.DefaultRuntime
import zio.config.magnolia.ConfigDescriptorProvider._

object TypesafeConfigHoconList extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsDetails(accounts: List[Account], database: Database, users: List[String])

  val listHocon =
    """
    accounts = [
      {
          region : us-east
          accountId: jon
      }
      {
          region : us-west
          accountId: chris
      }
      {
          region : us-some
          accountId: hello
      }
    ]

    users = ["vel", "muk", "riz"]

    database {
        port : 100
        url  : postgres
    }

    """

  val accountConfig =
    (string("region") |@| string("accountId"))(Account.apply, Account.unapply)

  val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig) |@| list(string("users")))(
      AwsDetails.apply,
      AwsDetails.unapply
    )

  val listResult =
    read(awsDetailsConfig from hocon(Right(listHocon)))

  assert(
    runtime.unsafeRun(listResult) ==
      AwsDetails(
        List(Account("us-east", "jon"), Account("us-west", "chris"), Account("us-some", "hello")),
        Database(100, "postgres"),
        List("vel", "muk", "riz")
      )
  )

  // If defining such a configuration description is tedious, you may rely on (experimental) magnolia module
  val automaticAwsDetailsConfig = description[AwsDetails]

  assert(
    runtime.unsafeRun(read(automaticAwsDetailsConfig from hocon(Right(listHocon)))) ==
      AwsDetails(
        List(Account("us-east", "jon"), Account("us-west", "chris"), Account("us-some", "hello")),
        Database(100, "postgres"),
        List("vel", "muk", "riz")
      )
  )
}
