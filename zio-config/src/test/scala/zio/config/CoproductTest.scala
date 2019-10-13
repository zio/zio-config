package zio.config

import zio.{ IO, ZIO }
import zio.config.Config._
import zio.config.CoproductTestUtils._
import zio.config.helpers._
import zio.config.ReadErrors.ReadError._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object CoproductTest
    extends BaseSpec(
      suite("Coproduct support")(
        testM("left element satisfied") {
          checkM(genTestParams) { p =>
            assertM(readLeft(p), isLeft(equalTo(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl)))))
          }
        },
        testM("right element satisfied") {
          checkM(genTestParams) { p =>
            assertM(readRight(p), isRight(equalTo(PasswordAuth(p.vUser, p.vCount, p.vFactor))))
          }
        },
        testM("should accumulate all errors") {
          checkM(genTestParams) { p =>
            val expected = ReadErrors(
              MissingValue(p.kLdap),
              ParseError(p.kFactor, "notafloat", "float")
            )

            assertM(readWithErrors(p), isLeft(equalTo(expected)))
          }
        },
        testM("left and right both populated should choose left") {
          checkM(genTestParams) { p =>
            assertM(readChooseLeftFromBoth(p), isLeft(equalTo(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl)))))
          }
        }
      )
    )

object CoproductTestUtils {
  final case class Ldap(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class PasswordAuth(user: String, count: Int, factor: Float)

  final case class TestParams(
    kLdap: String,
    vLdap: String,
    kDbUrl: String,
    vDbUrl: String,
    kUser: String,
    vUser: String,
    kCount: String,
    vCount: Int,
    kFactor: String,
    vFactor: Float
  )

  val genTestParams: Gen[Random, TestParams] =
    for {
      kLdap       <- genSymbol(1, 20)
      vLdap       <- genNonEmptyString(50)
      kDbUrl      <- genSymbol(1, 20).filter(s => s != kLdap)
      vDbUrl      <- genNonEmptyString(50)
      kUser       <- genSymbol(1, 20).filter(s => s != kLdap && s != kDbUrl)
      vUser       <- genNonEmptyString(50)
      kCount      <- genSymbol(1, 20).filter(s => s != kLdap && s != kDbUrl && s != kUser)
      vCount      <- Gen.anyInt
      kDbUrlLocal <- genSymbol(1, 20).filter(s => s != kLdap && s != kDbUrl && s != kUser && s != kCount)
      vDbUrlLocal <- Gen.anyFloat
    } yield TestParams(kLdap, vLdap, kDbUrl, vDbUrl, kUser, vUser, kCount, vCount, kDbUrlLocal, vDbUrlLocal)

  def readLeft(p: TestParams): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor))(PasswordAuth.apply, PasswordAuth.unapply)

    val authConfig = enterprise.orElseEither(password)

    read(authConfig).provide(ConfigSource.fromMap(Map(p.kLdap -> p.vLdap, p.kDbUrl -> p.vDbUrl)))
  }

  def readRight(p: TestParams): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor))(PasswordAuth.apply, PasswordAuth.unapply)

    val authConfig = enterprise.orElseEither(password)

    read(authConfig).provide(
      ConfigSource.fromMap(Map(p.kUser -> p.vUser, p.kCount -> p.vCount.toString, p.kFactor -> p.vFactor.toString))
    )
  }

  def readWithErrors(
    p: TestParams
  ): ZIO[Any, Nothing, Either[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]]] = {
    val enterprise =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor))(PasswordAuth.apply, PasswordAuth.unapply)

    val authConfig = enterprise.orElseEither(password)

    read(authConfig)
      .provide(
        ConfigSource.fromMap(
          Map(
            p.kDbUrl  -> p.vDbUrl,
            p.kUser   -> p.vUser,
            p.kCount  -> p.vCount.toString,
            p.kFactor -> "notafloat"
          )
        )
      )
      .either
  }

  def readChooseLeftFromBoth(p: TestParams): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor))(PasswordAuth.apply, PasswordAuth.unapply)

    val authConfig = enterprise.orElseEither(password)

    read(authConfig)
      .provide(
        ConfigSource.fromMap(
          Map(
            p.kLdap   -> p.vLdap,
            p.kDbUrl  -> p.vDbUrl,
            p.kUser   -> p.vUser,
            p.kCount  -> p.vCount.toString,
            p.kFactor -> p.vFactor.toString
          )
        )
      )
  }
}
