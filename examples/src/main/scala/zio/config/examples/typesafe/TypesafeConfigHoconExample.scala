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
          details = [
            {
              country = [
                {
                  name : [a]
                  code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }

                {
                  name : [b]  
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }
              ]
            }
          ]
      }

      {
          details = [
            {
              country = [
                {
                  name : [c]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }

                {
                  name : [d]  
                   code = [
                     {
                       x : 1
                       y : [1, 2, 3]
                     }
                  ]
                }
              ]
            }
          ]
      }
      {
          details = [
            {
              country = [
                {
                  name : [e]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }

                {
                  name : [f]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }
              ]
            }

            {
              country = [
                {
                  name : [g]
                   code = [
                     {
                       x : 1
                       y : [1, 2, 3]
                     }
                  ]
                }

                {
                  name : [h, i]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }
               {
                  name : [j]
                   code = [
                     {
                       x : 1
                       y : [1, 2, 3, 4]
                     }
                  ]
                }

                 {
                  name : [k]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }

                {
                  name : [l, m]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }
               {
                  name : [n]
                   code = [
                     {
                       x : 1
                       y : [1]
                     }
                  ]
                }
              ]
            }
          ]
      }
    ]
    """

  final case class Code(x: Int, y: Option[List[Int]])
  final case class Country(name: List[String], code: List[Code])
  final case class Details(country: List[Country])
  final case class AccountDetails(details: List[Details])

  final case class Accounts(accounts: List[AccountDetails])

  val codeConfig = (int("x") |@| list(int("y")).optional)(Code.apply, Code.unapply)

  val countryConfig = (list(string("name")) |@| nested("code")(list(codeConfig)))(Country.apply, Country.unapply)

  val accountDetailsConfig =
    nested("country")(list(countryConfig).xmap(Details.apply)(_.country))

  val accountConfig =
    nested("details")(list(accountDetailsConfig)).xmap(AccountDetails.apply)(
      _.details
    )

  val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)

  val awsDetailsConfig =
    (nested("accounts")(list(accountConfig))).xmap(
      Accounts.apply
    )(
      _.accounts
    )

  val listResult =
    read(awsDetailsConfig from hocon(Right(listHocon)))

  println(
    listResult
  )

  assert(
    listResult ==
      Right(
        Accounts(
          List(
            AccountDetails(
              List(
                Details(
                  List(
                    Country(List("a"), List(Code(1, Some(List(1))))),
                    Country(List("b"), List(Code(1, Some(List(1)))))
                  )
                )
              )
            ),
            AccountDetails(
              List(
                Details(
                  List(
                    Country(List("c"), List(Code(1, Some(List(1))))),
                    Country(List("d"), List(Code(1, Some(List(1, 2, 3)))))
                  )
                )
              )
            ),
            AccountDetails(
              List(
                Details(
                  List(
                    Country(List("e"), List(Code(1, Some(List(1))))),
                    Country(List("f"), List(Code(1, Some(List(1)))))
                  )
                ),
                Details(
                  List(
                    Country(List("g"), List(Code(1, Some(List(1, 2, 3))))),
                    Country(List("h", "i"), List(Code(1, Some(List(1))))),
                    Country(List("j"), List(Code(1, Some(List(1, 2, 3, 4))))),
                    Country(List("k"), List(Code(1, Some(List(1))))),
                    Country(List("l", "m"), List(Code(1, Some(List(1))))),
                    Country(List("n"), List(Code(1, Some(List(1)))))
                  )
                )
              )
            )
          )
        )
      )
  )
}
