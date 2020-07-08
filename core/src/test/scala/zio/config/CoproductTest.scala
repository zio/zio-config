package zio.config

import zio.config.ConfigDescriptor._
import ReadError._
import zio.config.helpers._
import CoproductTestUtils._
import zio.config.ReadError.Step.Key
import zio.random.Random
import zio.test.Assertion._
import zio.test._

import scala.concurrent.duration.Duration

object CoproductTest
    extends BaseSpec(
      suite("Coproduct support")(
        testM("left element satisfied") {
          check(genTestParams) { p =>
            assert(readLeft(p))(isRight(equalTo(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))))
          }
        },
        testM("right element satisfied") {
          check(genTestParams) { p =>
            assert(readRight(p))(
              isRight(equalTo(Right(PasswordAuth(p.vUser, p.vCount, p.vFactor, Duration(p.vCodeValid)))))
            )
          }
        },
        testM("should accumulate all errors") {
          check(genTestParams) { p =>
            val expected: ReadError[String] =
              OrErrors(
                List(
                  ZipErrors(List(MissingValue(List(Key(p.kLdap)), List("value of type string"))), Set()),
                  ZipErrors(
                    List(
                      ZipErrors(
                        List(
                          FormatError(
                            List(Key(p.kFactor)),
                            "Provided value is notafloat, expecting the type float"
                          )
                        )
                      )
                    )
                  )
                )
              )

            assert(readWithErrors(p))(isLeft(equalTo(expected)))
          }
        },
        testM("left and right both populated should choose left") {
          check(genTestParams) { p =>
            assert(readChooseLeftFromBoth(p))(isRight(equalTo(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl))))))
          }
        }
      )
    )

object CoproductTestUtils {
  final case class Ldap(value: String) extends AnyVal
  final case class EnterpriseAuth(ldap: Ldap, dburl: DbUrl)
  final case class PasswordAuth(user: String, count: Int, factor: Float, codeValid: Duration)

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
    vFactor: Float,
    kCodeValid: String,
    vCodeValid: String
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
      kCValid <- genNonEmptyString(15).filter(
                  s => s != kLdap && s != kDbUrl && s != kUser && s != kCount && s != kDbUrlLocal
                )
      vCValid <- genDuration(5)
    } yield TestParams(
      kLdap,
      vLdap,
      kDbUrl,
      vDbUrl,
      kUser,
      vUser,
      kCount,
      vCount,
      kDbUrlLocal,
      vDbUrlLocal,
      kCValid,
      vCValid
    )

  def readLeft(p: TestParams): Either[ReadError[String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap)(Ldap.apply, Ldap.unapply) |@| string(p.kDbUrl)(DbUrl.apply, DbUrl.unapply))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor) |@| duration(p.kCodeValid))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )

    val authConfig = enterprise.orElseEither(password)

    read(
      authConfig from ConfigSource.fromMap(Map(p.kLdap -> p.vLdap, p.kDbUrl -> p.vDbUrl))
    )
  }

  def readRight(p: TestParams) = {
    val enterprise =
      (string(p.kLdap)(Ldap.apply, Ldap.unapply) |@| string(p.kDbUrl)(DbUrl.apply, DbUrl.unapply))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor) |@| duration(p.kCodeValid))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )

    val authConfig = enterprise.orElseEither(password)

    read(
      authConfig from
        ConfigSource.fromMap(
          Map(
            p.kUser      -> p.vUser,
            p.kCount     -> p.vCount.toString,
            p.kFactor    -> p.vFactor.toString,
            p.kCodeValid -> p.vCodeValid
          )
        )
    )
  }

  def readWithErrors(
    p: TestParams
  ): Either[ReadError[String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap)(Ldap.apply, Ldap.unapply) |@| string(p.kDbUrl)(DbUrl.apply, DbUrl.unapply))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor) |@| duration(p.kCodeValid))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )

    val authConfig = enterprise.orElseEither(password)

    read(
      authConfig from
        ConfigSource.fromMap(
          Map(
            p.kDbUrl     -> p.vDbUrl,
            p.kUser      -> p.vUser,
            p.kCount     -> p.vCount.toString,
            p.kFactor    -> "notafloat",
            p.kCodeValid -> p.vCodeValid
          )
        )
    )
  }

  def readChooseLeftFromBoth(p: TestParams) = {
    val enterprise =
      (string(p.kLdap)(Ldap.apply, Ldap.unapply) |@| string(p.kDbUrl)(DbUrl.apply, DbUrl.unapply))(
        EnterpriseAuth.apply,
        EnterpriseAuth.unapply
      )

    val password =
      (string(p.kUser) |@| int(p.kCount) |@| float(p.kFactor) |@| duration(p.kCodeValid))(
        PasswordAuth.apply,
        PasswordAuth.unapply
      )

    val authConfig = enterprise.orElseEither(password)

    read(
      authConfig from
        ConfigSource.fromMap(
          Map(
            p.kLdap      -> p.vLdap,
            p.kDbUrl     -> p.vDbUrl,
            p.kUser      -> p.vUser,
            p.kCount     -> p.vCount.toString,
            p.kFactor    -> p.vFactor.toString,
            p.kCodeValid -> p.vCodeValid
          )
        )
    )
  }
}
