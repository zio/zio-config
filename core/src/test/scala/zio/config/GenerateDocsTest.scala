package zio.config

import zio.config.string._
import zio.config.ConfigDocs.{ Leaf, Nested, Zip }
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
            assert(generateDocs(p.descriptor from p.source))(equalTo(p.docs))
          }
        },
        testM("generate docs with value") {
          check(generateDocsParams) { p =>
            assert(generateReport(p.descriptor from p.source, p.value))(equalTo(p.docsWithValue))
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

    def descriptor: ConfigDescriptor[AppConfig] = {
      val credentials = (string(keys.user) ?? "Example: ZioUser" |@| string(keys.password) ?? "Example: ZioPass")(
        Credentials.apply,
        Credentials.unapply
      ) ?? "Credentials"

      val database = (int(keys.port) ?? "Example: 8088" |@| string(keys.url) ?? "Example: abc.com")(
        Database.apply,
        Database.unapply
      ) ?? "Database"

      (string(keys.secret).optional ?? "Application secret" |@| nested(keys.credentials)(credentials) |@| nested(
        keys.database
      )(database))(
        AppConfig.apply,
        AppConfig.unapply
      )
    }

    def source: ConfigSource[String, String] = {
      val source = Seq(
        s"${keys.credentials}.${keys.user}"     -> value.credentials.user,
        s"${keys.credentials}.${keys.password}" -> value.credentials.password,
        s"${keys.database}.${keys.url}"         -> value.database.url,
        s"${keys.database}.${keys.port}"        -> value.database.port.toString
      )
      ConfigSource.fromMap(value.secret.fold(source)(v => (keys.secret -> v) +: source).toMap, "test")
    }

    def docs: ConfigDocs[String, String] =
      Zip(
        Zip(
          Nested(
            keys.secret,
            Leaf(
              (Set(ConfigSource.Name("test"))),
              List("value of type string", "optional value", "Application secret")
            )
          ),
          Nested(
            keys.credentials,
            Zip(
              Nested(
                keys.user,
                Leaf(
                  (Set(ConfigSource.Name("test"))),
                  List("value of type string", "Example: ZioUser", "Credentials")
                )
              ),
              Nested(
                keys.password,
                Leaf(
                  (Set(ConfigSource.Name("test"))),
                  List("value of type string", "Example: ZioPass", "Credentials")
                )
              )
            )
          )
        ),
        Nested(
          keys.database,
          Zip(
            Nested(
              keys.port,
              Leaf(
                (Set(ConfigSource.Name("test"))),
                List("value of type int", "Example: 8088", "Database")
              )
            ),
            Nested(
              keys.url,
              Leaf(
                (Set(ConfigSource.Name("test"))),
                List("value of type string", "Example: abc.com", "Database")
              )
            )
          )
        )
      )

    def docsWithValue: Either[String, ConfigDocs[String, String]] =
      Right(
        Zip(
          Zip(
            Nested(
              keys.secret,
              Leaf(
                (Set(ConfigSource.Name("test"))),
                List("value of type string", "optional value", "Application secret"),
                value.secret
              )
            ),
            Nested(
              keys.credentials,
              Zip(
                Nested(
                  keys.user,
                  Leaf(
                    (Set(ConfigSource.Name("test"))),
                    List("value of type string", "Example: ZioUser", "Credentials"),
                    Some(value.credentials.user)
                  )
                ),
                Nested(
                  keys.password,
                  Leaf(
                    (Set(ConfigSource.Name("test"))),
                    List("value of type string", "Example: ZioPass", "Credentials"),
                    Some(value.credentials.password)
                  )
                )
              )
            )
          ),
          Nested(
            keys.database,
            Zip(
              Nested(
                keys.port,
                Leaf(
                  (Set(ConfigSource.Name("test"))),
                  List("value of type int", "Example: 8088", "Database"),
                  Some(value.database.port).map(_.toString)
                )
              ),
              Nested(
                keys.url,
                Leaf(
                  (Set(ConfigSource.Name("test"))),
                  List("value of type string", "Example: abc.com", "Database"),
                  Some(value.database.url)
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
