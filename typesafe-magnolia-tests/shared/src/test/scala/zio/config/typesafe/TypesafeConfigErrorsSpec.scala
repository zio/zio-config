package zio.config.typesafe

import zio.config._
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}

import magnolia._
import ConfigDescriptor._

object TypesafeConfigErrorsSpec extends DefaultRunnableSpec {
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  val configNestedAutomatic: ConfigDescriptor[AwsConfig] = descriptor[AwsConfig]

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

  val spec: ZSpec[Environment, Failure] = suite("TypesafeConfig Error")(
    testM("A variant error case with typesafe HOCON config and a magnolia description") {
      val nestedConfigAutomaticResult1 =
        read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithStringDb))

      val nestedConfigAutomaticExpect1 = AwsConfig(Account("us-east", "jon"), Some(Right("hi")))

      val nestedConfigAutomaticResult2 =
        read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithDb))

      val nestedConfigAutomaticExpect2 =
        AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres"))))

      val nestedConfigAutomaticResult3 =
        read(configNestedAutomatic from TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll))

      val nestedConfigAutomaticExpect3 = AwsConfig(Account("us-east", "jon"), None)

      val result =
        nestedConfigAutomaticResult1.zip(nestedConfigAutomaticResult2).zip(nestedConfigAutomaticResult3)

      assertM(result)(
        equalTo(((nestedConfigAutomaticExpect1, nestedConfigAutomaticExpect2), nestedConfigAutomaticExpect3))
      )
    },
    // test("A variant error case with a not well-formed typesafe HOCON config") {
    //   val hocconStringWithParseError =
    //     s"""
    //      account {
    //     """

    //   val notWellFormedConfigResult = TypesafeConfigSource.fromHoconString(hocconStringWithParseError)

    //   assert(notWellFormedConfigResult.isLeft)(Assertion.isTrue)
    // },
    testM("A variant error case with typesafe HOCON config and a manual description") {
      val configNestedManual = {
        val accountConfig  =
          (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
        val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)
        (nested("account")(accountConfig) |@|
          (nested("database")(databaseConfig).orElseEither(string("database"))).optional)(
          AwsConfig.apply,
          AwsConfig.unapply
        )
      }
      val nestedConfigManualResult1 =
        read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithDb))

      val nestedConfigManualExpect1 =
        AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres"))))

      val nestedConfigManualResult2 =
        read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithStringDb))

      val nestedConfigManualExpect2 =
        AwsConfig(Account("us-east", "jon"), Some(Right("hi")))

      val nestedConfigManualResult3 =
        read(configNestedManual from TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll))

      val nestedConfigManualExpect3 = AwsConfig(Account("us-east", "jon"), None)

      val result =
        nestedConfigManualResult1.zip(nestedConfigManualResult2).zip(nestedConfigManualResult3)

      assertM(result)(equalTo((nestedConfigManualExpect1, nestedConfigManualExpect2), nestedConfigManualExpect3))
    },
    testM("A substitution case with typesafe HOCON config and a magnolia description") {
      val hoconStringWithSubstitution =
        """
        datacentergeneric = { clustersize = 6 }
        datacentereast = ${datacentergeneric} { name = "east" }
        datacenterwest = ${datacentergeneric} { name = "west", clustersize = 8 }
        """

      final case class Details(clustersize: Int, name: String)
      final case class DatabaseDetails(datacenterwest: Details, datacentereast: Details)

      val configWithHoconSubstitution = descriptor[DatabaseDetails]

      val substitutionResult =
        read(configWithHoconSubstitution from TypesafeConfigSource.fromHoconString(hoconStringWithSubstitution))

      val expect = DatabaseDetails(Details(8, "west"), Details(6, "east"))

      assertM(substitutionResult)(equalTo(expect))
    }
  )
}
