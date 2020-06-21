package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
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
                reread <- ZIO.fromEither(
                           read(cId from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("newtype 2 roundtrip") {
          checkM(genDbUrl) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cDbUrl, p))
                reread <- ZIO.fromEither(
                           read(cDbUrl from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("case class 1 roundtrip") {
          checkM(genEnterpriseAuth) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cEnterpriseAuth, p))
                reread <- ZIO.fromEither(
                           read(
                             cEnterpriseAuth from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid)
                           )
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("nested case class roundtrip") {
          checkM(genNestedConfig) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cNestedConfig, p))
                reread <- ZIO.fromEither(
                           read(
                             cNestedConfig from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid)
                           )
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("single field case class roundtrip") {
          checkM(genSingleField) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cSingleField, p))
                reread <- ZIO.fromEither(
                           read(cSingleField from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid))
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        },
        testM("coproduct roundtrip") {
          checkM(genCoproductConfig) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cCoproductConfig, p))
                reread <- ZIO.fromEither(
                           read(
                             cCoproductConfig from ConfigSource.fromPropertyTree(written, "test", LeafForSequence.Valid)
                           )
                         )
              } yield reread

            assertM(p2)(equalTo(p))
          }
        }
      )
    )

object ReadWriteRoundtripTestUtils {
  final case class CoproductConfig(coproduct: Either[DataItem, NestedPath])
  final case class DataItem(oid: Option[Id], count: Int)
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedPath(enterpriseAuth: EnterpriseAuth, count: Int, factor: Float)
  final case class SingleField(count: Int)

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

  val genNestedConfig: Gen[Random, NestedPath] =
    for {
      auth   <- genEnterpriseAuth
      count  <- Gen.anyInt
      factor <- Gen.anyFloat
    } yield NestedPath(auth, count, factor)

  val genSingleField: Gen[Random, SingleField] =
    for {
      count <- Gen.anyInt
    } yield SingleField(count)

  val genCoproductConfig: Gen[Random, CoproductConfig] =
    Gen.either(genDataItem, genNestedConfig).map(CoproductConfig)

  val cId             = string("kId")(Id.apply, Id.unapply)
  val cId2            = string("kId2")(Id.apply, Id.unapply)
  val cDataItem       = (cId2.optional |@| int("kDiCount"))(DataItem.apply, DataItem.unapply)
  val cDbUrl          = string("kDbUrl")(DbUrl.apply, DbUrl.unapply)
  val cEnterpriseAuth = (cId |@| cDbUrl)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

  val cNestedConfig =
    (cEnterpriseAuth |@| int("kCount") |@| float("kFactor"))(NestedPath.apply, NestedPath.unapply)

  val cSingleField: ConfigDescriptor[SingleField] =
    int("kCount")(SingleField.apply, SingleField.unapply)

  val cCoproductConfig =
    (cDataItem.orElseEither(cNestedConfig))(CoproductConfig.apply, CoproductConfig.unapply)
}
