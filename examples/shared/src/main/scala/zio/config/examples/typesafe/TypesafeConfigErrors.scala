package zio.config.examples.typesafe

import zio.IO
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.examples.ZioOps
import zio.Config, Config._
import zio.ConfigProvider

object TypesafeConfigErrors extends App {
  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Database])

  val configNestedAutomatic: Config[AwsConfig] =
    deriveConfig[AwsConfig]

  val hocconStringWithDb: String =
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

  val hocconStringWithNoDatabaseAtAll: String =
    s"""
    account {
        region : us-east
        accountId: jon
    }
   """

  // Port is invalid
  val hocconStringWithDbWithParseError: String =
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

  val nestedConfigAutomaticResult2: IO[Config.Error, AwsConfig] =
    read(configNestedAutomatic from ConfigProvider.fromHoconString(hocconStringWithDb))

  val nestedConfigAutomaticResult3: IO[Config.Error, AwsConfig] =
    read(configNestedAutomatic from ConfigProvider.fromHoconString(hocconStringWithNoDatabaseAtAll))

  assert(
    nestedConfigAutomaticResult3 equalM
      AwsConfig(Account("us-east", "jon"), None)
  )

  val configNestedManual: Config[AwsConfig] = {
    val accountConfig  =
      (string("region") zip string("accountId")).to[Account]
    val databaseConfig =
      (int("port") zip string("url")).to[Database]
    ((accountConfig.nested("account")) zip (databaseConfig
      .nested("database"))
      .optional).to[AwsConfig]
  }

  val nestedConfigManualResult1: IO[Config.Error, AwsConfig] =
    read(configNestedManual from ConfigProvider.fromHoconString(hocconStringWithDb))

  val nestedConfigManualResult3: IO[Config.Error, AwsConfig] =
    read(configNestedManual from ConfigProvider.fromHoconString(hocconStringWithNoDatabaseAtAll))

  assert(
    nestedConfigManualResult3 equalM
      AwsConfig(Account("us-east", "jon"), None)
  )

  // Substitution Example, Example from typesafe/config documentation
  val hoconStringWithSubstitution =
    """
    datacentergeneric = { clustersize = 6 }
    datacentereast = ${datacentergeneric} { name = "east" }
    datacenterwest = ${datacentergeneric} { name = "west", clustersize = 8 }
    """

  final case class Details(clustersize: Int, name: String)
  final case class DatabaseDetails(datacenterwest: Details, datacentereast: Details)

  val configWithHoconSubstitution: Config[DatabaseDetails] = deriveConfig[DatabaseDetails]

  val finalResult: IO[Config.Error, DatabaseDetails] =
    read(configWithHoconSubstitution from ConfigProvider.fromHoconString(hoconStringWithSubstitution))

  assert(
    finalResult equalM
      DatabaseDetails(Details(8, "west"), Details(6, "east"))
  )
}
