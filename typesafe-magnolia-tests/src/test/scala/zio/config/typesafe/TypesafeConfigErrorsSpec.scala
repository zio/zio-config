package zio.config.typesafe

import zio.config.ConfigDescriptor.{ int, nested, string }
import zio.config.ReadError.Step.Key
import zio.config.ReadError.{ FormatError, OrErrors }
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.read
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, suite, test, _ }

object TypesafeConfigErrorsSpec extends DefaultRunnableSpec {
  final case class Account(region: String, accountId: String)
  final case class Database(port: Int, url: String)
  final case class AwsConfig(account: Account, database: Option[Either[Database, String]])

  val configNestedAutomatic = descriptor[AwsConfig]

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

  val spec = suite("TypesafeConfig Error")(
    test("A variant error case with typesafe HOCON config and a magnolia description") {
      val nestedConfigAutomaticResult1 =
        TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedAutomatic from source)
        }
      val nestedConfigAutomaticExpect1 = Right(AwsConfig(Account("us-east", "jon"), Some(Right("hi"))))

      val nestedConfigAutomaticResult2 =
        TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedAutomatic from source)
        }
      val nestedConfigAutomaticExpect2 =
        Right(AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))

      val nestedConfigAutomaticResult3 =
        TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedAutomatic from source)
        }
      val nestedConfigAutomaticExpect3 = Right(AwsConfig(Account("us-east", "jon"), None))

      val nestedConfigAutomaticResult4 = TypesafeConfigSource.fromHoconString(hocconStringWithDbWithParseError) match {
        case Left(value)   => Left(value)
        case Right(source) => read(configNestedAutomatic from source)
      }
      val nestedConfigAutomaticExpect4 = Left(
        OrErrors(
          List(
            FormatError(List(Key("database"), Key("port")), "Provided value is 1ab200, expecting the type int"),
            FormatError(List(Key("database")), "Provided value is of type Record, expecting the type Leaf")
          )
        )
      )

      assert(nestedConfigAutomaticResult1)(equalTo(nestedConfigAutomaticExpect1)) &&
      assert(nestedConfigAutomaticResult2)(equalTo(nestedConfigAutomaticExpect2)) &&
      assert(nestedConfigAutomaticResult3)(equalTo(nestedConfigAutomaticExpect3)) &&
      assert(nestedConfigAutomaticResult4)(equalTo(nestedConfigAutomaticExpect4))
    },
    test("A variant error case with a not well-formed typesafe HOCON config") {
      val hocconStringWithParseError =
        s"""
         account {
        """

      val notWellFormedConfigResult = TypesafeConfigSource.fromHoconString(hocconStringWithParseError)

      assert(notWellFormedConfigResult.isLeft)(Assertion.isTrue)
    },
    test("A variant error case with typesafe HOCON config and a manual description") {
      val configNestedManual = {
        val accountConfig =
          (string("region") |@| string("accountId"))(Account.apply, Account.unapply)
        val databaseConfig = (int("port") |@| string("url"))(Database.apply, Database.unapply)
        (nested("account")(accountConfig) |@| nested("database")(databaseConfig)
          .orElseEither(string("database"))
          .optional)(
          AwsConfig.apply,
          AwsConfig.unapply
        )
      }
      val nestedConfigManualResult1 =
        TypesafeConfigSource.fromHoconString(hocconStringWithDb) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedManual from source)
        }
      val nestedConfigManualExpect1 =
        Right(AwsConfig(Account("us-east", "jon"), Some(Left(Database(1200, "postgres")))))

      val nestedConfigManualResult2 =
        TypesafeConfigSource.fromHoconString(hocconStringWithStringDb) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedManual from source)
        }
      val nestedConfigManualExpect2 = Right(AwsConfig(Account("us-east", "jon"), Some(Right("hi"))))

      val nestedConfigManualResult3 =
        TypesafeConfigSource.fromHoconString(hocconStringWithNoDatabaseAtAll) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configNestedManual from source)
        }
      val nestedConfigManualExpect3 = Right(AwsConfig(Account("us-east", "jon"), None))

      assert(nestedConfigManualResult1)(equalTo(nestedConfigManualExpect1)) &&
      assert(nestedConfigManualResult2)(equalTo(nestedConfigManualExpect2)) &&
      assert(nestedConfigManualResult3)(equalTo(nestedConfigManualExpect3))
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
        TypesafeConfigSource.fromHoconString(hoconStringWithSubstitution) match {
          case Left(value)   => Left(value)
          case Right(source) => read(configWithHoconSubstitution from source)
        }
      val expect = Right(DatabaseDetails(Details(8, "west"), Details(6, "east")))

      assert(substitutionResult)(equalTo(expect))
    }
  )
}
