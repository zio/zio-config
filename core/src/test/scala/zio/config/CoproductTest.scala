package zio.config

import zio.config.ConfigDescriptor._
import ReadError._
import zio.config.helpers._
import CoproductTestUtils._
import zio.config.ReadError.Step.Key
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import VersionSpecificSupport._

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
        testM("round trip of enum works") {
          check(genSealedTraitParams) { sourceMap =>
            val source     = ConfigSource.fromMap(sourceMap, keyDelimiter = Some('.'))
            val readResult = read(Z.config from source)
            val writeResult = readResult.swap
              .map(_.prettyPrint())
              .swap
              .flatMap(
                r => r.toMap(Z.config).map(_.mapValues(_.mkString).toMap)
              )

            assert(writeResult)(equalTo(Right[String, Map[String, String]](sourceMap)))
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

  sealed trait Z

  object Z {

    val aConfig: ConfigDescriptor[A] = string("a")(A.apply, A.unapply)

    val bConfig: ConfigDescriptor[B.type] =
      string.transformOrFail(a => if (a == "b") Right(B) else Left("Can only be b"), _ => Right("b"))

    val cConfig: ConfigDescriptor[C] =
      (int("c1") |@| string("c2"))(C.apply, C.unapply)

    val dConfig: ConfigDescriptor[D] =
      int("d")(D.apply, D.unapply)

    val eConfig: ConfigDescriptor[E] =
      double("e")(E.apply, E.unapply)

    val fConfig: ConfigDescriptor[F.type] =
      string.transformOrFail(a => if (a == "f") Right(F) else Left("Can only be b"), _ => Right("f"))

    val gConfig: ConfigDescriptor[G.type] =
      string.transformOrFail(a => if (a == "g") Right(G) else Left("Can only be b"), _ => Right("g"))

    val hConfig: ConfigDescriptor[H.type] =
      string.transformOrFail(a => if (a == "h") Right(H) else Left("Can only be b"), _ => Right("h"))

    val iConfig: ConfigDescriptor[I] =
      double("i")(I.apply, I.unapply)

    val config =
      nested("z")(enumeration[Z](aConfig, bConfig, cConfig, dConfig, eConfig, fConfig, gConfig, hConfig, iConfig))

    case class A(a: String)           extends Z
    case object B                     extends Z
    case class C(c1: Int, c2: String) extends Z
    case class D(d: Int)              extends Z
    case class E(e: Double)           extends Z
    case object F                     extends Z
    case object G                     extends Z
    case object H                     extends Z
    case class I(i: Double)           extends Z
  }

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

  val genSealedTraitParams =
    for {
      s1     <- Gen.alphaNumericStringBounded(1, 10)
      s2     <- Gen.alphaNumericStringBounded(1, 10)
      int1   <- Gen.int(0, 100)
      int2   <- Gen.int(0, 100)
      double <- Gen.double(0, 100)
      result <- Gen.oneOf(
                 Gen.const(Map("z.a" -> s1)),
                 // hardcoded b because the value needs to be "b" for case-object B
                 Gen.const(Map("z"    -> "b")),
                 Gen.const(Map("z.c1" -> int1.toString, "z.c2" -> s2)),
                 Gen.const(Map("z.d"  -> int2.toString)),
                 Gen.const(Map("z.e"  -> double.toString)),
                 // hardcoded values because the value needs to be "f", "g", and "h" for case-object F, G, and H respectively
                 Gen.const(Map("z"   -> "f")),
                 Gen.const(Map("z"   -> "g")),
                 Gen.const(Map("z"   -> "h")),
                 Gen.const(Map("z.i" -> double.toString))
               )

    } yield result

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
