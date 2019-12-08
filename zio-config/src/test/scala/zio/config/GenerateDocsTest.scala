package zio.config

import zio.config.ConfigDescriptor.{ int, nested, string }
import zio.config.ConfigDocs.Details.{ Descriptions, DescriptionsWithValue }
import zio.config.ConfigDocs.{ Both, NestedPath, Path }
import zio.config.ConfigSource.{ ConstantMap, EmptySource }
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
            assert(generateDocs(p.descriptor from p.source), equalTo(p.docs))
          }
        },
        testM("generate docs with value") {
          check(generateDocsParams) { p =>
            assert(generateDocsWithValue(p.descriptor from p.source, p.value), equalTo(p.docsWithValue))
          }
        }
      )
    )

object GenerateDocsTestUtils {
  final case class Credentials(user: String, password: String)
  final case class Database(port: Int, url: String)
  final case class AppConfig(secret: Option[String], credentials: Credentials, database: Database)

  final case class KeyParams(
    secret: String,
    credentials: String,
    user: String,
    password: String,
    database: String,
    url: String,
    port: String
  )
  final case class GenerateDocsParams(keys: KeyParams, value: AppConfig) {

    def descriptor: ConfigDescriptor[String, String, AppConfig] = {
      val credentials = (string(keys.user) ? "Example: ZioUser" |@| string(keys.password) ? "Example: ZioPass")(
        Credentials.apply,
        Credentials.unapply
      ) ? "Credentials"

      val database = (int(keys.port) ? "Example: 8088" |@| string(keys.url) ? "Example: abc.com")(
        Database.apply,
        Database.unapply
      ) ? "Database"

      (string(keys.secret).optional ? "Application secret" |@| nested(keys.credentials)(credentials) |@| nested(
        keys.database
      )(database))(
        AppConfig.apply,
        AppConfig.unapply
      )
    }

    def source: ConfigSource[String, String] =
      ConfigSource.fromMap(
        Seq(
          value.secret.map(keys.secret -> _),
          Seq(
            s"${keys.credentials}.${keys.user}"     -> value.credentials.user,
            s"${keys.credentials}.${keys.password}" -> value.credentials.password
          ),
          Seq(
            s"${keys.database}.${keys.url}"  -> value.database.url,
            s"${keys.database}.${keys.port}" -> value.database.port.toString
          )
        ).flatten.toMap
      )

    def docs: ConfigDocs[String, String] =
      Both(
        Both(
          Path(
            keys.secret,
            Descriptions(List(EmptySource, ConstantMap, "value of type string", "optional value", "Application secret"))
          ),
          NestedPath(
            keys.credentials,
            Both(
              Path(
                keys.user,
                Descriptions(List(EmptySource, ConstantMap, "value of type string", "Example: ZioUser", "Credentials"))
              ),
              Path(
                keys.password,
                Descriptions(List(EmptySource, ConstantMap, "value of type string", "Example: ZioPass", "Credentials"))
              )
            )
          )
        ),
        NestedPath(
          keys.database,
          Both(
            Path(
              keys.port,
              Descriptions(List(EmptySource, ConstantMap, "value of type int", "Example: 8088", "Database"))
            ),
            Path(
              keys.url,
              Descriptions(List(EmptySource, ConstantMap, "value of type string", "Example: abc.com", "Database"))
            )
          )
        )
      )

    def docsWithValue: Either[String, ConfigDocs[String, String]] =
      Right(
        Both(
          Both(
            Path(
              keys.secret,
              DescriptionsWithValue(
                value.secret,
                List(EmptySource, ConstantMap, "value of type string", "optional value", "Application secret")
              )
            ),
            NestedPath(
              keys.credentials,
              Both(
                Path(
                  keys.user,
                  DescriptionsWithValue(
                    Some(value.credentials.user),
                    List(EmptySource, ConstantMap, "value of type string", "Example: ZioUser", "Credentials")
                  )
                ),
                Path(
                  keys.password,
                  DescriptionsWithValue(
                    Some(value.credentials.password),
                    List(EmptySource, ConstantMap, "value of type string", "Example: ZioPass", "Credentials")
                  )
                )
              )
            )
          ),
          NestedPath(
            keys.database,
            Both(
              Path(
                keys.port,
                DescriptionsWithValue(
                  Some(value.database.port).map(_.toString),
                  List(EmptySource, ConstantMap, "value of type int", "Example: 8088", "Database")
                )
              ),
              Path(
                keys.url,
                DescriptionsWithValue(
                  Some(value.database.url),
                  List(EmptySource, ConstantMap, "value of type string", "Example: abc.com", "Database")
                )
              )
            )
          )
        )
      )

  }

  private val genKey = genSymbol(1, 20)

  val genConfigKeys: Gen[Random, KeyParams] =
    for {
      kSecret      <- genKey
      kCredentials <- genKey
      kUser        <- genKey
      kPassword    <- genKey.filter(_ != kUser)
      kDatabase    <- genKey.filter(_ != kCredentials)
      kUrl         <- genKey
      kPort        <- genKey.filter(_ != kDatabase)
    } yield KeyParams(kSecret, kCredentials, kUser, kPassword, kDatabase, kUrl, kPort)

  private val genCredentials =
    for {
      user     <- genNonEmptyString(20)
      password <- genNonEmptyString(20)
    } yield Credentials(user, password)

  private val genDatabase =
    for {
      port <- Gen.anyInt
      url  <- genDbUrl
    } yield Database(port, url.value)

  private val genAppConfig =
    for {
      secret      <- Gen.option(genNonEmptyString(20))
      credentials <- genCredentials
      database    <- genDatabase
    } yield AppConfig(secret, credentials, database)

  val generateDocsParams: Gen[Random, GenerateDocsParams] =
    for {
      keys  <- genConfigKeys
      value <- genAppConfig
    } yield GenerateDocsParams(keys, value)
}
