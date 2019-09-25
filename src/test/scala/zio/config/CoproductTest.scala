package zio.config.examples

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import zio.DefaultRuntime
import zio.config.ConfigError.MissingValue
import zio.config._
import zio.config.testsupport.TestSupport

object CoproductTest extends
  Properties("Coproduct support")
  with TestSupport {

  private def symbol(minLength: Int, maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(minLength, maxLength)
        s <- Gen.listOfN(n, Gen.alphaChar).map(_.mkString)
    } yield s

  private def nonEmptyString(maxLength: Int): Gen[String] =
    for {
      n <- Gen.chooseNum(1, maxLength)
        s <- Gen.listOfN(n, Gen.asciiChar).map(_.mkString)
    } yield s

  ////

  private def genFor[A: Arbitrary]: Gen[A] =
    implicitly[Arbitrary[A]].arbitrary

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

  object TestParams {
    def gen: Gen[TestParams] =
      for {
        kLdap <- symbol(1, 20)
          vLdap <- nonEmptyString(50)
          kDbUrl <- symbol(1, 20).filterNot(s => s == kLdap)
          vDbUrl <- nonEmptyString(50)
          kUser <- symbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl)
          vUser <- nonEmptyString(50)
          kCount <- symbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl || s == kUser)
          vCount <- genFor[Int]
          kDbUrlLocal <- symbol(1, 20).filterNot(s => s == kLdap || s == kDbUrl || s == kUser || s == kCount)
          vDbUrlLocal <- genFor[Double]
      } yield TestParams(kLdap, vLdap, kDbUrl, vDbUrl, kUser, vUser, kCount, vCount, kDbUrlLocal, vDbUrlLocal)
  }

  property("left element satisfied") =
    forAll(TestParams.gen) {
      p =>
        testLeft(p)
          .shouldBe(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))
    }

  property("right element satisfied") =
    forAll(TestParams.gen) {
      p =>
        testRight(p)
          .shouldBe(Right(PasswordAuth(p.vUser, p.vCount, p.vFactor)))
    }

  property("should accumulate all errors") =
    forAll(TestParams.gen) {
      p =>
        testErrors(p)
          .shouldBe {
            Left(
              List(
                ConfigError(Seq(p.kLdap), MissingValue),
                ConfigError(Seq(p.kFactor), ConfigError.ParseError("notadouble", "double"))
              )
            )
          }
    }

  property("left and right both populated should choose left") =
    forAll(TestParams.gen) {
      p =>
        testChooseFromBoth(p)
          .shouldBe(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))
    }

  ////

  final case class Ldap(value: String) extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class PasswordAuth(user: String, Count: Int, factor: Double)

  val runtime = new DefaultRuntime {}

  def testLeft(p: TestParams): Either[EnterpriseAuth, PasswordAuth] = {
    val enterprise: Config[EnterpriseAuth] =
      (string(p.kLdap).map(Ldap) <*> string(p.kDbUrl).map(DbUrl)) (
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )
    val local: Config[PasswordAuth] = (string(p.kUser) <*> int(p.kCount) <*> double(p.kFactor)) (
      PasswordAuth.apply,
      PasswordAuth.unapply
    )
    val authConfig: Config[Either[EnterpriseAuth, PasswordAuth]] = enterprise or local

    val validConfigForSampleConfig: Map[String, String] =
      Map(
        p.kLdap -> p.vLdap,
        p.kDbUrl -> p.vDbUrl
      )

    runtime.unsafeRun(read(authConfig).run.provide(mapSource(validConfigForSampleConfig)))._2
  }

  def testRight(p: TestParams): Either[EnterpriseAuth, PasswordAuth] = {
    val enterprise: Config[EnterpriseAuth] = (string(p.kLdap).map(Ldap) <*> string(p.kDbUrl).map(DbUrl)) (
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
    val local: Config[PasswordAuth] = (string(p.kUser) <*> int(p.kCount) <*> double(p.kFactor)) (
      PasswordAuth.apply,
      PasswordAuth.unapply
    )
    val authConfig: Config[Either[EnterpriseAuth, PasswordAuth]] = enterprise or local

    val validConfigForAnotherConfig: Map[String, String] =
      Map(
        p.kUser -> p.vUser,
        p.kCount -> p.vCount.toString,
        p.kFactor -> p.vFactor.toString
      )

    runtime.unsafeRun(read(authConfig).run.provide(mapSource(validConfigForAnotherConfig)))._2
  }

  def testErrors(p: TestParams): Either[List[ConfigError], (ConfigReport, Either[EnterpriseAuth, PasswordAuth])] = {
    val enterprise: Config[EnterpriseAuth] = (string(p.kLdap).map(Ldap) <*> string(p.kDbUrl).map(DbUrl)) (
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
    val local: Config[PasswordAuth] = (string(p.kUser) <*> int(p.kCount) <*> double(p.kFactor)) (
      PasswordAuth.apply,
      PasswordAuth.unapply
    )
    val authConfig: Config[Either[EnterpriseAuth, PasswordAuth]] = enterprise or local

    val invalidConfig: Map[String, String] =
      Map(
        p.kDbUrl -> p.vDbUrl,
        p.kUser -> p.vUser,
        p.kCount -> p.vCount.toString,
        p.kFactor -> "notadouble"
      )

    runtime.unsafeRun(read(authConfig).run.provide(mapSource(invalidConfig)).either)
  }

  def testChooseFromBoth(p: TestParams): Either[EnterpriseAuth, PasswordAuth] = {
    val enterprise: Config[EnterpriseAuth] = (string(p.kLdap).map(Ldap) <*> string(p.kDbUrl).map(DbUrl)) (
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
    val local: Config[PasswordAuth] = (string(p.kUser) <*> int(p.kCount) <*> double(p.kFactor)) (
      PasswordAuth.apply,
      PasswordAuth.unapply
    )
    val authConfig: Config[Either[EnterpriseAuth, PasswordAuth]] = enterprise or local

    val allConfigsExist: Map[String, String] =
      Map(
        p.kLdap -> p.vLdap,
        p.kDbUrl -> p.vDbUrl,
        p.kUser -> p.vUser,
        p.kCount -> p.vCount.toString,
        p.kFactor -> p.vFactor.toString
      )

    runtime.unsafeRun(read(authConfig).run.provide(mapSource(allConfigsExist)))._2
  }
}
