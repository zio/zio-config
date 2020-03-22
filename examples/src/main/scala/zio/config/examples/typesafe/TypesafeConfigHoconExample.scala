package zio.config.examples.typesafe

import zio.DefaultRuntime
//import zio.config.ReadFunctions
import zio.config.ConfigDescriptor.{ int, list, nested, string }
//import zio.config.ReadError.ParseError
//import zio.config.magnolia.ConfigDescriptorProvider.description
import zio.config.read
import zio.config.typesafe.TypeSafeConfigSource.hocon

object TypesafeConfigHoconExample extends App {
  val runtime = new DefaultRuntime {}

  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  /*val configNestedAutomatic =
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
    read(configNestedAutomatic from hocon(Right(hocconStringWithStringDb)))

  val nestedConfigAutomaticResult2 =
    read(configNestedAutomatic from hocon(Right(hocconStringWithDb)))

  val nestedConfigAutomaticResult3 =
    read(configNestedAutomatic from hocon(Right(hocconStringWithNoDatabaseAtAll)))

  /*
  val nestedConfigAutomaticResult4 =
    read(configNestedAutomatic from hocon(Right(hocconStringWithDbWithParseError)))*/

  // println(nestedConfigAutomaticResult4)

  assert(nestedConfigAutomaticResult1 == Right(AwsConfig(Account("us-east", "jon"), Some(Right("hi")))))
  assert(
    nestedConfigAutomaticResult2 == Right(AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))
  )
  assert(nestedConfigAutomaticResult3 == Right(AwsConfig(Account("us-east", "jon"), None)))

  /*  assert(
    nestedConfigAutomaticResult4 == Left(
      ParseError(Vector("database", "port").toString, ReadFunctions.parseErrorMessage("1ab200", "int"))
    )
  )*/

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
    read(configNestedManual from hocon(Right(hocconStringWithDb)))

  val nestedConfigManualResult2 =
    read(configNestedManual from hocon(Right(hocconStringWithStringDb)))

  val nestedConfigManualResult3 =
    read(configNestedManual from hocon(Right(hocconStringWithNoDatabaseAtAll)))

  assert(
    nestedConfigManualResult1 == Right(AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))
  )
  assert(nestedConfigManualResult2 == Right(AwsConfig(Account("us-east", "jon"), Some(Right("hi")))))
  assert(nestedConfigManualResult3 == Right(AwsConfig(Account("us-east", "jon"), None)))

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

  assert(finalResult == Right(DatabaseDetails(Details(8, "west"), Details(6, "east"))))*/

  // List Example

  val listHocon =
    """
    accounts = [
      {
          region : x1
          accountId = [
            {
              bank : cba
              country = [
                {
                  name : aus
                  code : 10
                }
              ]
            
            }
            
            {
              bank : abc
              country = [
                {
                  name : sua
                  code : 20
                }
              ]
            }
          ]
      }
      {
          region : x2
          accountId = [
            {
              bank : cba
              country = [
                {
                  name : aus
                  code : 10
                }
              ]
            
            }
            
            {
              bank : abc
              country = [
                {
                  name : sua
                  code : 20
                }
              ]
            }
          ]
      }
      {
          region : x3
               accountId = [
            {
              bank : cba
              country = [
                {
                  name : aus
                  code : 10
                }
              ]
            
            }
            
            {
              bank : abc
              country = [
                {
                  name : sua
                  code : 20
                }
              ]
            }
          ]
      }
    ]

    database {
        port : 100
        url  : postgres
    }

    """

  final case class Country(name: String, code: Int)
  final case class AccountIdDetails(bank: String, country: List[Country])
  final case class AccountRegions(region: String, accountId: List[AccountIdDetails])

  final case class AwsDetails(accounts: List[AccountRegions], database: Database)

  val countryConfig = (string("name") |@| int("code"))(Country.apply, Country.unapply)

  val accountDetailsConfig =
    (string("bank") |@| nested("country")(list(countryConfig)))(AccountIdDetails.apply, AccountIdDetails.unapply)

  val accountConfig =
    (string("region") |@| nested("accountId")(list(accountDetailsConfig)))(
      AccountRegions.apply,
      AccountRegions.unapply
    )

  val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig)) |@| nested("database")(databaseConfig))(
      AwsDetails.apply,
      AwsDetails.unapply
    )

  val listResult =
    read(awsDetailsConfig from hocon(Right(listHocon)))

  println(listResult)
  /*  assert(
    listResult ==
      Right(
        AwsDetails(
          List(
            AccountRegions(List("x1", "y1", "z1"), "jon"),
            AccountRegions(List("x2", "y2", "z2"), "chris"),
            AccountRegions(List("x3", "y3", "z3"), "hello")
          ),
          Database(100, "postgres")
        )
      )
  )*/
}
