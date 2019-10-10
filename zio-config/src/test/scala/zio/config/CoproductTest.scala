package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.ReadError.{ MissingValue, ParseError }
import zio.config.testsupport.TestSupport
import zio.{ IO, ZIO }

object CoproductTest extends Properties("Coproduct support") with TestSupport {

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
    vFactor: Double
  )

  object TestParams extends TestSupport {
    def gen: Gen[TestParams] =
      for {
        kLdap       <- genSymbol(1, 20)
        vLdap       <- genNonEmptyString(50)
        kDbUrl      <- genSymbol(1, 20).filterNot(s => s == kLdap)
        vDbUrl      <- genNonEmptyString(50)
        kUser       <- genSymbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl)
        vUser       <- genNonEmptyString(50)
        kCount      <- genSymbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl || s == kUser)
        vCount      <- genFor[Int]
        kDbUrlLocal <- genSymbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl || s == kUser || s == kCount)
        vDbUrlLocal <- genFor[Double]
      } yield TestParams(kLdap, vLdap, kDbUrl, vDbUrl, kUser, vUser, kCount, vCount, kDbUrlLocal, vDbUrlLocal)
  }

  property("left element satisfied") = forAllZIO(TestParams.gen) { p =>
    readLeft(p)
      .shouldBe(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))
  }

  property("right element satisfied") = forAllZIO(TestParams.gen) { p =>
    readRight(p)
      .shouldBe(Right(PasswordAuth(p.vUser, p.vCount, p.vFactor)))
  }

  property("should accumulate all errors") = forAllZIO(TestParams.gen) { p =>
    readWithErrors(p).shouldBe {
      Left(
        ReadErrors(
          MissingValue(p.kLdap),
          ParseError(p.kFactor, "notadouble", "double")
        )
      )
    }
  }

  property("left and right both populated should choose left") = forAllZIO(TestParams.gen) { p =>
    readChooseLeftFromBoth(p)
      .shouldBe(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))
  }

  ////

  final case class Ldap(value: String)  extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class PasswordAuth(user: String, count: Int, factor: Double)

  import Config._

  private def readLeft(p: TestParams): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise: ConfigDescriptor[EnterpriseAuth] =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )
    val password: ConfigDescriptor[PasswordAuth] =
      (string(p.kUser) |@| int(p.kCount) |@| double(p.kFactor))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )
    val authConfig: ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]] =
      enterprise orElseEither password

    read(authConfig)
      .provide(
        ConfigSource.fromMap(
          Map(
            p.kLdap  -> p.vLdap,
            p.kDbUrl -> p.vDbUrl
          )
        )
      )
  }

  private def readRight(p: TestParams): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise: ConfigDescriptor[EnterpriseAuth] =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )
    val password: ConfigDescriptor[PasswordAuth] =
      (string(p.kUser) |@| int(p.kCount) |@| double(p.kFactor))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )
    val authConfig: ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]] =
      enterprise orElseEither password

    read(authConfig)
      .provide(
        ConfigSource.fromMap(
          Map(
            p.kUser   -> p.vUser,
            p.kCount  -> p.vCount.toString,
            p.kFactor -> p.vFactor.toString
          )
        )
      )
  }

  private def readWithErrors(
    p: TestParams
  ): ZIO[Any, Nothing, Either[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]]] = {
    val enterprise: ConfigDescriptor[EnterpriseAuth] =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )
    val password: ConfigDescriptor[PasswordAuth] =
      (string(p.kUser) |@| int(p.kCount) |@| double(p.kFactor))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )
    val authConfig: ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]] =
      enterprise orElseEither password

    read(authConfig)
      .provide(
        ConfigSource.fromMap(
          Map(
            p.kDbUrl  -> p.vDbUrl,
            p.kUser   -> p.vUser,
            p.kCount  -> p.vCount.toString,
            p.kFactor -> "notadouble"
          )
        )
      )
      .either
  }

  private def readChooseLeftFromBoth(
    p: TestParams
  ): IO[ReadErrors[String, String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise: ConfigDescriptor[EnterpriseAuth] =
      (string(p.kLdap).xmap(Ldap)(_.value) |@| string(p.kDbUrl).xmap(DbUrl)(_.value))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )
    val password: ConfigDescriptor[PasswordAuth] =
      (string(p.kUser) |@| int(p.kCount) |@| double(p.kFactor))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )
    val authConfig: ConfigDescriptor[Either[EnterpriseAuth, PasswordAuth]] =
      enterprise orElseEither password

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
