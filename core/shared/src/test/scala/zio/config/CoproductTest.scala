package zio.config

import com.github.ghik.silencer.silent
import zio.config.ConfigDescriptor._
import zio.config.PropertyTreePath.Step.Key
import zio.config.helpers._
import zio.test.Assertion._
import zio.test.{Sized, _}
import zio.{IO, Random, ZIO}

import scala.concurrent.duration.Duration

import ReadError._
import CoproductTestUtils._

@silent("Unused import")
object CoproductTest extends BaseSpec {
  import scala.collection.compat._
  import VersionSpecificSupport._

  val spec: Spec[TestConfig with Random with Sized with Any, TestFailure[Serializable], TestSuccess] =
    suite("Coproduct support")(
      test("left element satisfied") {
        check(genTestParams) { p =>
          assertM(readLeft(p))(equalTo(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl)))))
        }
      },
      test("right element satisfied") {
        check(genTestParams) { p =>
          assertM(readRight(p))(
            equalTo(Right(PasswordAuth(p.vUser, p.vCount, p.vFactor, Duration(p.vCodeValid))))
          )
        }
      },
      test("round trip of enum works") {
        check(genSealedTraitParams) { sourceMap =>
          val source = ConfigSource.fromMap(sourceMap, keyDelimiter = Some('.'))

          val writeResult =
            for {
              readResult  <- read(Z.config from source).either
              writeResult <- ZIO.fromEither(
                               readResult.swap
                                 .map(_.prettyPrint())
                                 .swap
                                 .flatMap(r => r.toMap(Z.config).map(_.view.mapValues(_.mkString).toMap))
                             )
            } yield writeResult

          assertM(writeResult)(equalTo(sourceMap))
        }
      },
      test("should accumulate all errors") {
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
                          "Provided value is notafloat, expecting the type float",
                          List("value of type float")
                        )
                      )
                    )
                  )
                )
              )
            )

          assertM(readWithErrors(p).either)(isLeft(equalTo(expected)))
        }
      },
      test("left and right both populated should choose left") {
        check(genTestParams) { p =>
          assertM(readChooseLeftFromBoth(p))(equalTo(Left(EnterpriseAuth(Ldap(p.vLdap), DbUrl(p.vDbUrl)))))
        }
      }
    )
}

object CoproductTestUtils {

  sealed trait Z

  object Z {

    val aConfig: ConfigDescriptor[A] = string("a").to[A]

    val bConfig: ConfigDescriptor[B.type] =
      string.transformOrFail(a => if (a == "b") Right(B) else Left("Can only be b"), _ => Right("b"))

    val cConfig: ConfigDescriptor[C] =
      (int("c1") zip string("c2")).to[C]

    val dConfig: ConfigDescriptor[D] =
      int("d").to[D]

    val eConfig: ConfigDescriptor[E] =
      double("e").to[E]

    val fConfig: ConfigDescriptor[F.type] =
      string.transformOrFail(a => if (a == "f") Right(F) else Left("Can only be b"), _ => Right("f"))

    val gConfig: ConfigDescriptor[G.type] =
      string.transformOrFail(a => if (a == "g") Right(G) else Left("Can only be b"), _ => Right("g"))

    val hConfig: ConfigDescriptor[H.type] =
      string.transformOrFail(a => if (a == "h") Right(H) else Left("Can only be b"), _ => Right("h"))

    val iConfig: ConfigDescriptor[I] =
      double("i").to[I]

    val config: ConfigDescriptor[Z] =
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

  final case class Ldap(value: String)
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

  val genSealedTraitParams: Gen[Random with Sized, Map[String, String]] =
    for {
      s1     <- Gen.alphaNumericStringBounded(1, 10)
      s2     <- Gen.alphaNumericStringBounded(1, 10)
      int1   <- Gen.int(0, 100)
      int2   <- Gen.int(0, 100)
      double <- Gen.double(0, 100)
      result <- Gen.oneOf(
                  Gen.const(Map("z.a" -> s1)),
                  // hardcoded b because the value needs to be "b" for case-object B
                  Gen.const(Map("z" -> "b")),
                  Gen.const(Map("z.c1" -> int1.toString, "z.c2" -> s2)),
                  Gen.const(Map("z.d" -> int2.toString)),
                  Gen.const(Map("z.e" -> double.toString)),
                  // hardcoded values because the value needs to be "f", "g", and "h" for case-object F, G, and H respectively
                  Gen.const(Map("z" -> "f")),
                  Gen.const(Map("z" -> "g")),
                  Gen.const(Map("z" -> "h")),
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
      vCount      <- Gen.int
      kDbUrlLocal <- genSymbol(1, 20).filter(s => s != kLdap && s != kDbUrl && s != kUser && s != kCount)
      vDbUrlLocal <- Gen.float
      kCValid     <-
        genNonEmptyString(15).filter(s => s != kLdap && s != kDbUrl && s != kUser && s != kCount && s != kDbUrlLocal)
      vCValid     <- genDuration(5)
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

  def readLeft(p: TestParams): IO[ReadError[String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).to[Ldap] zip string(p.kDbUrl).to[DbUrl]).to[EnterpriseAuth]

    val password =
      (string(p.kUser) zip int(p.kCount) zip float(p.kFactor) zip duration(p.kCodeValid)).to[PasswordAuth]

    val authConfig = enterprise.orElseEither(password)

    read(
      authConfig from ConfigSource.fromMap(Map(p.kLdap -> p.vLdap, p.kDbUrl -> p.vDbUrl))
    )
  }

  def readRight(
    p: TestParams
  ): IO[ReadError[String], Either[CoproductTestUtils.EnterpriseAuth, CoproductTestUtils.PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).to[Ldap] zip string(p.kDbUrl).to[DbUrl]).to[EnterpriseAuth]

    val password =
      (string(p.kUser) zip int(p.kCount) zip float(p.kFactor) zip duration(p.kCodeValid)).to[PasswordAuth]

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
  ): IO[ReadError[String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).to[Ldap] zip string(p.kDbUrl).to[DbUrl]).to[EnterpriseAuth]

    val password =
      (string(p.kUser) zip int(p.kCount) zip float(p.kFactor) zip duration(p.kCodeValid)).to[PasswordAuth]

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

  def readChooseLeftFromBoth(p: TestParams): IO[ReadError[String], Either[EnterpriseAuth, PasswordAuth]] = {
    val enterprise =
      (string(p.kLdap).to[Ldap] zip string(p.kDbUrl).to[DbUrl]).to[EnterpriseAuth]

    val password =
      (string(p.kUser) zip int(p.kCount) zip float(p.kFactor) zip duration(p.kCodeValid)).to[PasswordAuth]

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
