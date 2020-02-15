package zio.config.examples.typesafe

import zio.DefaultRuntime
import zio.config.ConfigDescriptor.{ int, list, nested, string }
import zio.config.ReadError.{ MissingValue, ParseError }
import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon

object TypesafeConfigHoconExample extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  private val configNestedAutomatic =
    description[AwsConfig]

  val hocconStringWithStringDb =
    s"""
    account {
        region : us-east
        accountId: jon
    }

    database = "hi"
   """

  val hocconStringWithDb =
    s"""
    account {
        region : us-east
        accountId: jon
    }

    database {
       url  : "postgres"
       port : 1200
    }
   """

  val hocconStringWithNoDatabaseAtAll =
    s"""
    account {
        region : us-east
        accountId: jon
    }
   """

  // Port is invalid
  val hocconStringWithDbWithParseError =
    s"""
    account {
        region : us-east
        accountId: jon
    }
      database {
       url  : "postgres"
       port : 1ab200
    }
   """

  val nestedConfigAutomaticResult1 =
    runtime.unsafeRun(read(configNestedAutomatic from hocon(Right(hocconStringWithStringDb))))

  val nestedConfigAutomaticResult2 =
    runtime.unsafeRun(read(configNestedAutomatic from hocon(Right(hocconStringWithDb))))

  val nestedConfigAutomaticResult3 =
    runtime.unsafeRun(read(configNestedAutomatic from hocon(Right(hocconStringWithNoDatabaseAtAll))))

  val nestedConfigAutomaticResult4 =
    runtime.unsafeRun(read(configNestedAutomatic from hocon(Right(hocconStringWithDbWithParseError))).either)

  assert(nestedConfigAutomaticResult1 == AwsConfig(Account("us-east", "jon"), Some(Right("hi"))))
  assert(nestedConfigAutomaticResult2 == AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))
  assert(nestedConfigAutomaticResult3 == AwsConfig(Account("us-east", "jon"), None))
  println(nestedConfigAutomaticResult4)
  assert(
    nestedConfigAutomaticResult4 == Left(
      ::(ParseError(Vector("database", "port"), "1ab200", "int"), ::(MissingValue(Vector("database"), Some(0)), Nil))
    )
  )

  val configNestedManual = {
    val accountConfig =
      (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
    val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)
    (nested("account")(accountConfig) |@| nested("database")(databaseConfig).orElseEither(string("database")).optional)(
      AwsConfig.apply,
      AwsConfig.unapply
    )
  }

  val nestedConfigManualResult1 =
    runtime.unsafeRun(read(configNestedManual from hocon(Right(hocconStringWithDb))))

  val nestedConfigManualResult2 =
    runtime.unsafeRun(read(configNestedManual from hocon(Right(hocconStringWithStringDb))))

  val nestedConfigManualResult3 =
    runtime.unsafeRun(read(configNestedManual from hocon(Right(hocconStringWithNoDatabaseAtAll))))

  assert(nestedConfigManualResult1 == AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))
  assert(nestedConfigManualResult2 == AwsConfig(Account("us-east", "jon"), Some(Right("hi"))))
  assert(nestedConfigManualResult3 == AwsConfig(Account("us-east", "jon"), None))

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

  println(runtime.unsafeRun(listResult))

  assert(
    runtime.unsafeRun(listResult) ==
      AwsDetails(
        List(Account("us-east", "jon"), Account("us-west", "chris"), Account("us-some", "hello")),
        Database(100, "postgres")
      )
  )
}
