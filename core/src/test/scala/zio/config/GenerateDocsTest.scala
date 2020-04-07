package zio.config

import zio.config.ConfigDescriptor.{ int, nested, string }
import zio.config.ConfigDocs.{ Both, Leaf, NestedPath, Sources }
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
            assert(generateDocsWithValue(p.descriptor from p.source, p.value))(equalTo(p.docsWithValue))
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
      Both(
        Both(
          NestedPath(
            keys.secret,
            Leaf(
              Sources(List("test")),
              List("value of type string", "optional value", "Application secret")
            )
          ),
          NestedPath(
            keys.credentials,
            Both(
              NestedPath(
                keys.user,
                Leaf(
                  Sources(List("test")),
                  List("value of type string", "Example: ZioUser", "Credentials")
                )
              ),
              NestedPath(
                keys.password,
                Leaf(
                  Sources(List("test")),
                  List("value of type string", "Example: ZioPass", "Credentials")
                )
              )
            )
          )
        ),
        NestedPath(
          keys.database,
          Both(
            NestedPath(
              keys.port,
              Leaf(
                Sources(List("test")),
                List("value of type int", "Example: 8088", "Database")
              )
            ),
            NestedPath(
              keys.url,
              Leaf(
                Sources(List("test")),
                List("value of type string", "Example: abc.com", "Database")
              )
            )
          )
        )
      )

    def docsWithValue: Either[String, ConfigDocs[String, String]] =
      Right(
        Both(
          Both(
            NestedPath(
              keys.secret,
              Leaf(
                Sources(List("test")),
                List("value of type string", "optional value", "Application secret"),
                value.secret
              )
            ),
            NestedPath(
              keys.credentials,
              Both(
                NestedPath(
                  keys.user,
                  Leaf(
                    Sources(List("test")),
                    List("value of type string", "Example: ZioUser", "Credentials"),
                    Some(value.credentials.user)
                  )
                ),
                NestedPath(
                  keys.password,
                  Leaf(
                    Sources(List("test")),
                    List("value of type string", "Example: ZioPass", "Credentials"),
                    Some(value.credentials.password)
                  )
                )
              )
            )
          ),
          NestedPath(
            keys.database,
            Both(
              NestedPath(
                keys.port,
                Leaf(
                  Sources(List("test")),
                  List("value of type int", "Example: 8088", "Database"),
                  Some(value.database.port).map(_.toString)
                )
              ),
              NestedPath(
                keys.url,
                Leaf(
                  Sources(List("test")),
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
