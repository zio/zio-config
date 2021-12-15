package zio.config.examples.typesafe

import zio.IO
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe._

import ConfigDescriptor._

object TypesafeConfigErrors extends App {
  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  val configNestedAutomatic: ConfigDescriptor[AwsConfig] =
    descriptor[AwsConfig]

  val hocconStringWithStringDb: String =
    s"""
    account {
        region : us-east
        accountId: jon
    }

    database = "hi"
   """

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

  val nestedConfigAutomaticResult1: IO[ReadError[String], AwsConfig] =
    read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithStringDb))

  val nestedConfigAutomaticResult2: IO[ReadError[String], AwsConfig] =
    read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithDb))

  val nestedConfigAutomaticResult3: IO[ReadError[String], AwsConfig] =
    read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll))

  assert(
    nestedConfigAutomaticResult1 equalM
      AwsConfig(Account("us-east", "jon"), Some(Right("hi")))
  )

  assert(
    nestedConfigAutomaticResult2 equalM
      AwsConfig(
        Account("us-east", "jon"),
        Some(Left(Database(1200, "postgres")))
      )
  )
  assert(
    nestedConfigAutomaticResult3 equalM
      AwsConfig(Account("us-east", "jon"), None)
  )

  val configNestedManual: ConfigDescriptor[AwsConfig] = {
    val accountConfig  =
      (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
    val databaseConfig =
      (int("port") |@| string("url"))(Database.apply, Database.unapply)
    (nested("account")(accountConfig) |@| nested("database")(databaseConfig)
      .orElseEither(string("database"))
      .optional)(AwsConfig.apply, AwsConfig.unapply)
  }

  val nestedConfigManualResult1: IO[ReadError[String], AwsConfig] =
    read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithDb))

  val nestedConfigManualResult2: IO[ReadError[String], AwsConfig] =
    read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithStringDb))

  val nestedConfigManualResult3: IO[ReadError[String], AwsConfig] =
    read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll))

  assert(
    nestedConfigManualResult1 equalM
      AwsConfig(
        Account("us-east", "jon"),
        Some(Left(Database(1200, "postgres")))
      )
  )
  assert(
    nestedConfigManualResult2 equalM
      AwsConfig(Account("us-east", "jon"), Some(Right("hi")))
  )
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

  val configWithHoconSubstitution: ConfigDescriptor[DatabaseDetails] = descriptor[DatabaseDetails]

  val finalResult: IO[ReadError[String], DatabaseDetails] =
    read(configWithHoconSubstitution from TypesafeConfigSource.fromHoconString(hoconStringWithSubstitution))

  assert(
    finalResult equalM
      DatabaseDetails(Details(8, "west"), Details(6, "east"))
  )
}
