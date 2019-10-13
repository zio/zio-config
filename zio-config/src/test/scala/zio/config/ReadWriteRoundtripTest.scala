package zio.config

import zio.ZIO
import zio.config.Config._
import zio.config.helpers._
import zio.config.ReadWriteRoundtripTestUtils._
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object ReadWriteRoundtripTest
    extends BaseSpec(
      suite("Coproduct support")(
        testM("newtype 1 roundtrip") {
          checkM(genId) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cId, p))
                reread  <- read(cId).provide(ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("newtype 2 roundtrip") {
          checkM(genDbUrl) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cDbUrl, p))
                reread  <- read(cDbUrl).provide(ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("case class 1 roundtrip") {
          checkM(genEnterpriseAuth) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cEnterpriseAuth, p))
                reread  <- read(cEnterpriseAuth).provide(ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("nested case class roundtrip") {
          checkM(genNestedConfig) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cNestedConfig, p))
                reread  <- read(cNestedConfig).provide(ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("coproduct roundtrip") {
          checkM(genCoproductConfig) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cCoproductConfig, p))
                reread  <- read(cCoproductConfig).provide(ConfigSource.fromPropertyTree(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        }
      )
    )

object ReadWriteRoundtripTestUtils {
  final case class CoproductConfig(coproduct: Either[DataItem, NestedConfig])
  final case class DataItem(oid: Option[Id], count: Int)
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedConfig(enterpriseAuth: EnterpriseAuth, count: Int, factor: Float)

  val genDataItem: Gen[Random, DataItem] =
    for {
      oid   <- Gen.option(genId)
      count <- Gen.anyInt
    } yield DataItem(oid, count)

  val genEnterpriseAuth: Gen[Random, EnterpriseAuth] =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)

  val genNestedConfig: Gen[Random, NestedConfig] =
    for {
      auth   <- genEnterpriseAuth
      count  <- Gen.anyInt
      factor <- Gen.anyFloat
    } yield NestedConfig(auth, count, factor)

  val genCoproductConfig: Gen[Random, CoproductConfig] =
    Gen.either(genDataItem, genNestedConfig).map(CoproductConfig)

  val cId             = string("kId").xmap(Id)(_.value)
  val cId2            = string("kId2").xmap(Id)(_.value)
  val cDataItem       = (cId2.optional |@| int("kDiCount"))(DataItem.apply, DataItem.unapply)
  val cDbUrl          = string("kDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuth = (cId |@| cDbUrl)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

  val cNestedConfig =
    (cEnterpriseAuth |@| int("kCount") |@| float("kFactor"))(NestedConfig.apply, NestedConfig.unapply)

  val cCoproductConfig =
    (cDataItem.orElseEither(cNestedConfig)).xmap(CoproductConfig)(_.coproduct)
}
