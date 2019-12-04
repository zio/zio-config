package zio.config

import zio.config.ConfigDescriptor.{ int, string }
import zio.config.ConfigDocs.Details.{ Descriptions, DescriptionsWithValue }
import zio.config.ConfigDocs.{ Both, Path }
import zio.config.GenerateDocsTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object GenerateDocsTest
    extends BaseSpec(
      suite("Generate docs")(
        testM("generate docs") {
          check(generateDocsParams) { p =>
            assert(generateDocs(p.config), equalTo(p.docs))
          }
        },
        testM("generate docs with value") {
          check(generateDocsParams) { p =>
            assert(generateDocsWithValue(p.config, p.appConfig), equalTo(p.docsWithValue))
          }
        }
      )
    )

object GenerateDocsTestUtils {

  final case class Credentials(user: String, password: String)

  final case class Database(port: Int, url: String)

  final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

  final case class GenerateDocsParams(appConfig: AppConfig) {

    def config: ConfigDescriptor[String, String, AppConfig] = {
      val credentials =
        (string("USER") ? "Example: ZioUser" |@|
          string("PASSWORD") ? "Example: ZioPass")(Credentials.apply, Credentials.unapply) ? "Credentials"

      val database = (int("PORT") ? "Example: 8088" |@|
        string("URL") ? "Example: abc.com")(Database.apply, Database.unapply) ? "Database"

      (string("APP_SECRET").optional ? "Application secret" |@|
        credentials |@| database)(AppConfig.apply, AppConfig.unapply)
    }

    def docs: ConfigDocs[String, String] =
      Both(
        Both(
          Path("APP_SECRET",
               Descriptions(List("<empty>", "value of type string", "optional value", "Application secret"))),
          Both(
            Path("USER", Descriptions(List("<empty>", "value of type string", "Example: ZioUser", "Credentials"))),
            Path("PASSWORD", Descriptions(List("<empty>", "value of type string", "Example: ZioPass", "Credentials")))
          )
        ),
        Both(
          Path("PORT", Descriptions(List("<empty>", "value of type int", "Example: 8088", "Database"))),
          Path("URL", Descriptions(List("<empty>", "value of type string", "Example: abc.com", "Database")))
        )
      )

    def docsWithValue: Either[String, ConfigDocs[String, String]] =
      Right(
        Both(
          Both(
            Path(
              "APP_SECRET",
              DescriptionsWithValue(
                appConfig.secret,
                List("<empty>", "value of type string", "optional value", "Application secret")
              )
            ),
            Both(
              Path(
                "USER",
                DescriptionsWithValue(
                  Some(appConfig.credentials.user),
                  List("<empty>", "value of type string", "Example: ZioUser", "Credentials")
                )
              ),
              Path(
                "PASSWORD",
                DescriptionsWithValue(
                  Some(appConfig.credentials.password),
                  List("<empty>", "value of type string", "Example: ZioPass", "Credentials")
                )
              )
            )
          ),
          Both(
            Path(
              "PORT",
              DescriptionsWithValue(
                Some(appConfig.database.port).map(_.toString),
                List("<empty>", "value of type int", "Example: 8088", "Database")
              )
            ),
            Path(
              "URL",
              DescriptionsWithValue(
                Some(appConfig.database.url),
                List("<empty>", "value of type string", "Example: abc.com", "Database")
              )
            )
          )
        )
      )

  }

  private val genCredentials: Gen[Random, Credentials] =
    for {
      user     <- genNonEmptyString(20)
      password <- genNonEmptyString(20)
    } yield Credentials(user, password)

  private val genDatabase: Gen[Random, Database] =
    for {
      port <- Gen.anyInt
      url  <- genDbUrl
    } yield Database(port, url.value)

  private val genAppConfig: Gen[Random, AppConfig] =
    for {
      secret      <- Gen.option(genNonEmptyString(20))
      credentials <- genCredentials
      database    <- genDatabase
    } yield AppConfig(secret, credentials, database)

  val generateDocsParams: Gen[Random, GenerateDocsParams] =
    for {
      config <- genAppConfig
    } yield GenerateDocsParams(config)

}
