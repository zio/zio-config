package zio.config.typesafe

import zio.config._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}
import zio.Config
import magnolia._
import Config._

final case class Account(region: String, accountId: String)
final case class Database(port: Int, url: String)
final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

object TypesafeConfigErrorsSpec extends ZIOSpecDefault {
  val configNestedAutomatic: Config[AwsConfig] = deriveConfig[AwsConfig]

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

  def spec: Spec[Any, Config.Error] = suite("TypesafeConfig Error")(
    test("A variant error case with typesafe HOCON config and a magnolia description") {
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

      val expected = (nestedConfigAutomaticExpect1, nestedConfigAutomaticExpect2, nestedConfigAutomaticExpect3)

      assertZIO(result)(equalTo(expected))
    },
    // test("A variant error case with a not well-formed typesafe HOCON config") {
    //   val hocconStringWithParseError =
    //     s"""
    //      account {
    //     """

    //   val notWellFormedConfigResult = TypesafeConfigSource.fromHoconString(hocconStringWithParseError)

    //   assert(notWellFormedConfigResult.isLeft)(Assertion.isTrue)
    // },
    test("A variant error case with typesafe HOCON config and a manual description") {
      val configNestedManual = {
        val accountConfig =
          (string("region") zip string("accountId")).to[Account]

        val databaseConfig =
          (int("port") zip string("url")).to[Database]

        ((accountConfig.nested("account")) zip
          ((databaseConfig.nested("database")).orElseEither(string("database"))).optional).to[AwsConfig]
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

      val expected =
        (nestedConfigManualExpect1, nestedConfigManualExpect2, nestedConfigManualExpect3)

      assertZIO(result)(equalTo(expected))
    },
    test("A substitution case with typesafe HOCON config and a magnolia description") {
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

      assertZIO(substitutionResult)(equalTo(expect))
    }
  )
}
