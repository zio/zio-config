package zio.config

import zio.config.ConfigDescriptor.{ int, nested, string }
import zio.config.NestedConfigTestUtils._
import zio.config.PropertyTree.{ Leaf, Record }
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
            assert(write(p.config, p.value), isRight(equalTo(p.record)))
          }
        }
      )
    )

object NestedConfigTestUtils {
  final case class Credentials(user: String, password: String)
  final case class Database(url: DbUrl, credentials: Credentials)
  final case class AppConfig(db: Database, port: Int)

  final case class TestParams(
    kPort: String,
    vPort: Int,
    kDatabase: String,
    kDbUrl: String,
    vDbUrl: DbUrl,
    kCredentials: String,
    kUser: String,
    vUser: String,
    kPassword: String,
    vPassword: String
  ) {
    def value = AppConfig(Database(vDbUrl, Credentials(vUser, vPassword)), vPort)

    def config: ConfigDescriptor[String, String, AppConfig] = {
      val credentials = (string(kUser) |@| string(kPassword))(Credentials.apply, Credentials.unapply)

      val database =
        (string(kDbUrl).xmap(DbUrl)(_.value) |@| nested(kCredentials)(credentials))(Database.apply, Database.unapply)

      (nested(kDatabase)(database) |@| int(kPort))(AppConfig, AppConfig.unapply)
    }

    def record = Record(
      Map(
        kDatabase -> Record(
          Map(
            kDbUrl       -> Leaf(vDbUrl.value),
            kCredentials -> Record(Map(kUser -> Leaf(vUser), kPassword -> Leaf(vPassword)))
          )
        ),
        kPort -> Leaf(vPort.toString)
      )
    )

    def source: ConfigSource[String, String] = ConfigSource.fromMap(
      Map(
        s"$kDatabase.$kDbUrl"                  -> vDbUrl.value,
        s"$kDatabase.$kCredentials.$kPassword" -> vPassword,
        s"$kDatabase.$kCredentials.$kUser"     -> vUser,
        kPort                                  -> vPort.toString
      )
    )
  }

  private val genKey = genSymbol(1, 20)

  val genNestedConfigParams: Gen[Random, TestParams] =
    for {
      kPort        <- genKey
      vPort        <- Gen.anyInt
      kDb          <- genKey.filter(_ != kPort)
      kDbUrl       <- genKey
      vDbUrl       <- genNonEmptyString(50).map(DbUrl)
      kCredentials <- genKey.filter(_ != kDbUrl)
      kUser        <- genKey
      vUser        <- genNonEmptyString(50)
      kPassword    <- genKey.filter(_ != kUser)
      vPassword    <- genNonEmptyString(50)
    } yield TestParams(kPort, vPort, kDb, kDbUrl, vDbUrl, kCredentials, kUser, vUser, kPassword, vPassword)
}
