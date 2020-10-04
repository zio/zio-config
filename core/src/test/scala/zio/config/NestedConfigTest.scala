package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.NestedConfigTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object NestedConfigTest
    extends BaseSpec(
      suite("Nested config")(
        testM("read") {
          check(genNestedConfigParams) { p =>
            assert(read(p.config.from(p.source)))(isRight(equalTo(p.value)))
          }
        },
        testM("write") {
          check(genNestedConfigParams) { p =>
            assert(write(p.config, p.value).map(_.flattenString()))(
              isRight(equalTo(toMultiMap(p.map)))
            )
          }
        },
        testM("nested with default") {
          val config = string("x").default("y")
          val r = ZIO.fromEither(read(config from ConfigSource.fromPropertyTree(PropertyTree.empty, "test", LeafForSequence.Valid)))

          assertM(r)(equalTo("y"))
        }
      )
    )

object NestedConfigTestUtils {
  final case class Credentials(user: String, password: String)
  final case class DbConnection(host: String, port: Int)
  final case class Database(connection: Either[DbUrl, DbConnection], credentials: Option[Credentials])
  final case class AppConfig(db: Database, pricing: Double)

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

  final case class TestParams(value: AppConfig) {

    val config: ConfigDescriptor[AppConfig] = {
      val credentials  = (string("user") |@| string("password"))(Credentials.apply, Credentials.unapply)
      val dbConnection = (string("host") |@| int("port"))(DbConnection.apply, DbConnection.unapply)

      val database =
        (string("dburl")(DbUrl.apply, DbUrl.unapply)
          .orElseEither(nested("connection")(dbConnection)) |@|
          nested("credentials")(credentials).optional)(Database.apply, Database.unapply)

      (nested("database")(database) |@| double("pricing"))(AppConfig, AppConfig.unapply)
    }

    val map =
      Seq(
        value.db.connection.fold(
          url => Seq("database.dburl" -> url.value),
          connection => {
            Seq(
              "database.connection.host" -> connection.host,
              "database.connection.port" -> connection.port.toString
            )
          }
        ),
        value.db.credentials.fold(Seq.empty[(String, String)]) { c =>
          Seq(
            "database.credentials.user"     -> c.user,
            "database.credentials.password" -> c.password
          )
        },
        Seq("pricing" -> value.pricing.toString)
      ).flatten.toMap

    val source: ConfigSource =
      ConfigSource.fromMap(map, keyDelimiter = Some('.'))
  }

  val genNestedConfigParams: Gen[Random, TestParams] =
    genAppConfig.map(TestParams)
}
