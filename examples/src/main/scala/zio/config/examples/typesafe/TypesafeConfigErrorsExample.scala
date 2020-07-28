package zio.config.examples.typesafe

import zio.config._, zio.config.typesafe._, ConfigDescriptor._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

object TypesafeConfigErrorsExample extends App {
  // A nested example with type safe config, and usage of magnolia
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  val configNestedAutomatic =
    descriptor[AwsConfig]

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
    TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedAutomatic from source)
    }

  val nestedConfigAutomaticResult2 =
    TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedAutomatic from source)
    }

  val nestedConfigAutomaticResult3 =
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

  val configNestedManual = {
    val accountConfig =
      (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
    val databaseConfig =
      (int("port") |@| string("url"))(Database.apply, Database.unapply)
    (nested("account")(accountConfig) |@| nested("database")(databaseConfig)
      .orElseEither(string("database"))
      .optional)(AwsConfig.apply, AwsConfig.unapply)
  }

  val nestedConfigManualResult1 =
    TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedManual from source)
    }

  val nestedConfigManualResult2 =
    TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
      case Left(value)   => Left(value)
      case Right(source) => read(configNestedManual from source)
    }

  val nestedConfigManualResult3 =
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

  val configWithHoconSubstitution = descriptor[DatabaseDetails]

  val finalResult =
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
