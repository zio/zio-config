package zio.config.examples.typesafe

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

  val nestedConfigAutomaticResult1: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedAutomatic from source)
    }

  val nestedConfigAutomaticResult2: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedAutomatic from source)
    }

  val nestedConfigAutomaticResult3: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedAutomatic from source)
    }

  assert(
    nestedConfigAutomaticResult1 == Right(
      AwsConfig(Account("us-east", "jon"), Some(Right("hi")))
    )
  )
  assert(
    nestedConfigAutomaticResult2 == Right(
      AwsConfig(
        Account("us-east", "jon"),
        Some(Left(Database(1200, "postgres")))
      )
    )
  )
  assert(
    nestedConfigAutomaticResult3 == Right(
      AwsConfig(Account("us-east", "jon"), None)
    )
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

  val nestedConfigManualResult1: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedManual from source)
    }

  val nestedConfigManualResult2: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedManual from source)
    }

  val nestedConfigManualResult3: Either[ReadError[String], AwsConfig] =
    TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedManual from source)
    }

  assert(
    nestedConfigManualResult1 == Right(
      AwsConfig(
        Account("us-east", "jon"),
        Some(Left(Database(1200, "postgres")))
      )
    )
  )
  assert(
    nestedConfigManualResult2 == Right(
      AwsConfig(Account("us-east", "jon"), Some(Right("hi")))
    )
  )
  assert(
    nestedConfigManualResult3 == Right(
      AwsConfig(Account("us-east", "jon"), None)
    )
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

  val finalResult: Either[ReadError[String], DatabaseDetails] =
    TypesafeConfigSource.fromHoconString(hoconStringWithSubstitution) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configWithHoconSubstitution from source)
    }

  assert(
    finalResult == Right(
      DatabaseDetails(Details(8, "west"), Details(6, "east"))
    )
  )
}
