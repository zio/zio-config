package zio.config

import zio.config.ConfigDescriptor.{ double, int, nested, string }
import zio.config.NestedConfigTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object NestedConfigTest
    extends BaseSpec(
      suite("Nested config")(
        testM("read") {
          checkM(genNestedConfigParams) { p =>
            assertM(read(p.config.from(p.source)).either, isRight(equalTo(p.value)))
          }
        },
        testM("write") {
          check(genNestedConfigParams) { p =>
            assert(
              write(p.config, p.value).map(_.flattenString()),
              isRight(equalTo(toMultiMap(p.map)))
            )
          }
        }
      )
    )

object NestedConfigTestUtils {
  final case class Credentials(user: String, password: String)
  final case class DbConnection(host: String, port: Int)
  final case class Database(connection: Either[DbUrl, DbConnection], credentials: Option[Credentials])
  final case class AppConfig(db: Database, pricing: Double)

  final case class KeyParams(
    host: String,
    port: String,
    user: String,
    password: String,
    connection: String,
    credentials: String,
    database: String,
    pricing: String
  )

  val genCredentials: Gen[Random, Credentials] =
    for {
      user     <- genNonEmptyString(20)
      password <- genNonEmptyString(20)
    } yield Credentials(user, password)

  val genDbConnection: Gen[Random, DbConnection] =
    for {
      host <- genNonEmptyString(20)
      port <- Gen.anyInt
    } yield DbConnection(host, port)

  val genDb: Gen[Random, Database] =
    for {
      connection  <- Gen.either(genNonEmptyString(20).map(DbUrl), genDbConnection)
      credentials <- Gen.option(genCredentials)
    } yield Database(connection, credentials)

  val genAppConfig: Gen[Random, AppConfig] =
    for {
      db      <- genDb
      pricing <- Gen.anyFloat.map(_.toDouble)
    } yield AppConfig(db, pricing)

  final case class TestParams(keys: KeyParams, value: AppConfig) {

    val config: ConfigDescriptor[String, String, AppConfig] = {
      val credentials  = (string(keys.user) |@| string(keys.password))(Credentials.apply, Credentials.unapply)
      val dbConnection = (string(keys.host) |@| int(keys.port))(DbConnection.apply, DbConnection.unapply)

      val database =
        (string(keys.connection).xmap(DbUrl)(_.value).orElseEither(nested(keys.connection)(dbConnection)) |@|
          nested(keys.credentials)(credentials).optional)(Database.apply, Database.unapply)

      (nested(keys.database)(database) |@| double(keys.pricing))(AppConfig, AppConfig.unapply)
    }

    val map =
      Seq(
        value.db.connection.fold(
          url => Seq(s"${keys.database}.${keys.connection}" -> url.value),
          connection => {
            val connectionKey = s"${keys.database}.${keys.connection}"
            Seq(
              s"$connectionKey.${keys.host}" -> connection.host,
              s"$connectionKey.${keys.port}" -> connection.port.toString
            )
          }
        ),
        value.db.credentials.fold(Seq.empty[(String, String)]) { c =>
          val credentialsKey = s"${keys.database}.${keys.credentials}"
          Seq(
            s"$credentialsKey.${keys.user}"     -> c.user,
            s"$credentialsKey.${keys.password}" -> c.password
          )
        },
        Seq(keys.pricing -> value.pricing.toString)
      ).flatten.toMap

    def source: ConfigSource[String, String] =
      ConfigSource.fromMap(map)
  }

  private val genKey = genSymbol(1, 20)

  val genConfigKeys: Gen[Random, KeyParams] =
    for {
      kHost        <- genKey
      kPort        <- genKey.filter(_ != kHost)
      kUser        <- genKey
      kPassword    <- genKey.filter(_ != kUser)
      kCredentials <- genKey
      kConnection  <- genKey.filter(_ != kCredentials)
      kDb          <- genKey
      kPricing     <- genKey.filter(_ != kDb)
    } yield KeyParams(kHost, kPort, kUser, kPassword, kConnection, kCredentials, kDb, kPricing)

  val genNestedConfigParams: Gen[Random, TestParams] =
    for {
      keys  <- genConfigKeys
      value <- genAppConfig
    } yield TestParams(keys, value)
}
