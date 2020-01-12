package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ int, list, nested, string }
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon

object TypesafeConfigHoconExample extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Database)

  private val configNestedAutomatic = description[AwsConfig]

  val hoconString =
    s"""
    account {
        region : us-east
        accountId: jon
    }

    database {
        port : 100
        url  : postgres
    }
   """

  val nestedConfigAutomaticResult =
    runtime.unsafeRun(read(configNestedAutomatic from hocon(Right(hoconString))))

  assert(nestedConfigAutomaticResult == AwsConfig(Account("us-east", "jon"), Database(100, "postgres")))

  val configNestedManual = {
    val accountConfig =
      (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
    val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)
    (nested("account")(accountConfig) |@| nested("database")(databaseConfig))(AwsConfig.apply, AwsConfig.unapply)
  }

  val nestedConfigManualResult =
    runtime.unsafeRun(read(configNestedManual from hocon(Right(hoconString))))

  assert(nestedConfigManualResult == AwsConfig(Account("us-east", "jon"), Database(100, "postgres")))

  // Substitution Example, Example from typesafe/config documentation
  val hoconStringWithSubstitution =
    """
    datacentergeneric = { clustersize = 6 }
    datacentereast = ${datacentergeneric} { name = "east" }
    datacenterwest = ${datacentergeneric} { name = "west", clustersize = 8 }
    """

  final case class Details(clustersize: Int, name: String)
  final case class DatabaseDetails(datacenterwest: Details, datacentereast: Details)

  val configWithHoconSubstitution = description[DatabaseDetails]

  val finalResult =
    read(configWithHoconSubstitution from hocon(Right(hoconStringWithSubstitution)))

  assert(runtime.unsafeRun(finalResult) == DatabaseDetails(Details(8, "west"), Details(6, "east")))

  // List Example

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

    database {
        port : 100
        url  : postgres
    }

    """

  final case class AwsDetails(accounts: List[Account], database: Database)

  val accountConfig =
    (string("region") |@| string("accountId"))(Account.apply, Account.unapply)

  val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig))(
      AwsDetails.apply,
      AwsDetails.unapply
    )

  val listResult =
    read(awsDetailsConfig from hocon(Right(listHocon)))

  assert(
    runtime.unsafeRun(listResult) ==
      AwsDetails(
        List(Account("us-east", "jon"), Account("us-west", "chris"), Account("us-some", "hello")),
        Database(100, "postgres")
      )
  )
}
