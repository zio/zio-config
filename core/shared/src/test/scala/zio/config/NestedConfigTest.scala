package zio.config

import zio.Has
import zio.config.ConfigDescriptor._
import zio.config.NestedConfigTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object NestedConfigTest extends BaseSpec {

  val spec: Spec[Has[TestConfig.Service] with Has[Random.Service], TestFailure[ReadError[String]], TestSuccess] =
    suite("Nested config")(
      testM("read") {
        checkM(genNestedConfigParams) { p =>
          assertM(read(p.config.from(p.source)))(equalTo(p.value))
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
        val r      =
          read(config from ConfigSource.fromPropertyTree(PropertyTree.empty, "test"))

        assertM(r)(equalTo("y"))
      }
    )
}

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
      connection  <- Gen.either(genNonEmptyString(20).map(DbUrl.apply), genDbConnection)
      credentials <- Gen.option(genCredentials)
    } yield Database(connection, credentials)

  val genAppConfig: Gen[Random, AppConfig] =
    for {
      db      <- genDb
      pricing <- Gen.anyFloat.map(_.toDouble)
    } yield AppConfig(db, pricing)

  final case class TestParams(value: AppConfig) {

    val config: ConfigDescriptor[AppConfig] = {
      val credentials  = (string("user") zip string("password")).to[Credentials]
      val dbConnection = (string("host") zip int("port")).to[DbConnection]

      val database =
        (string("dburl")
          .to[DbUrl]
          .orElseEither(nested("connection")(dbConnection)) zip
          nested("credentials")(credentials).optional).to[Database]

      (nested("database")(database) zip double("pricing")).to[AppConfig]
    }

    val map: Map[String, String] =
      Seq(
        value.db.connection.fold(
          url => Seq("database.dburl" -> url.value),
          connection =>
            Seq(
              "database.connection.host" -> connection.host,
              "database.connection.port" -> connection.port.toString
            )
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
    genAppConfig.map(TestParams.apply)
}
