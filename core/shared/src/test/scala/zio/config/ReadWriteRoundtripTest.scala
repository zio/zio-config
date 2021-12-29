package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ReadWriteRoundtripTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.{Has, ZIO}

object ReadWriteRoundtripTest extends BaseSpec {

  val spec: Spec[Has[TestConfig.Service] with Has[Random.Service], TestFailure[String], TestSuccess] =
    suite("Coproduct support")(
      testM("newtype 1 roundtrip") {
        checkM(genId) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cId, p))
              reread  <-
                read(cId from ConfigSource.fromPropertyTree(written, "test"))
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("newtype 2 roundtrip") {
        checkM(genDbUrl) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cDbUrl, p))
              reread  <-
                read(cDbUrl from ConfigSource.fromPropertyTree(written, "test"))
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("case class 1 roundtrip") {
        checkM(genEnterpriseAuth) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cEnterpriseAuth, p))
              reread  <-
                read(
                  cEnterpriseAuth from ConfigSource.fromPropertyTree(written, "test")
                )
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("nested case class roundtrip") {
        checkM(genNestedConfig) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cNestedConfig, p))
              reread  <-
                read(
                  cNestedConfig from ConfigSource.fromPropertyTree(written, "test")
                )
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("single field case class roundtrip") {
        checkM(genSingleField) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cSingleField, p))
              reread  <-
                read(cSingleField from ConfigSource.fromPropertyTree(written, "test"))
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("coproduct roundtrip") {
        checkM(genCoproductConfig) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cCoproductConfig, p))
              reread  <-
                read(
                  cCoproductConfig from ConfigSource.fromPropertyTree(written, "test")
                )
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      testM("empty sequence zipped with optional nested") {
        val config = (list("a")(string) zip nested("b")(string).optional)
        val data   = (Nil, None)
        val data2  = for {
          written <- ZIO.fromEither(write(config, data))
          reread  <-
            read(config from ConfigSource.fromPropertyTree(written, "test"))
              .mapError(_.getMessage)
        } yield reread

        assertM(data2)(equalTo(data))
      }
    )
}

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
    Gen.either(genDataItem, genNestedConfig).map(CoproductConfig.apply)

  val cId: ConfigDescriptor[Id]                         = string("kId").to[Id]
  val cId2: ConfigDescriptor[Id]                        = string("kId2").to[Id]
  val cDataItem: ConfigDescriptor[DataItem]             = (cId2.optional zip int("kDiCount")).to[DataItem]
  val cDbUrl: ConfigDescriptor[DbUrl]                   = string("kDbUrl").to[DbUrl]
  val cEnterpriseAuth: ConfigDescriptor[EnterpriseAuth] = (cId zip cDbUrl).to[EnterpriseAuth]

  val cNestedConfig: ConfigDescriptor[NestedPath] =
    (cEnterpriseAuth zip int("kCount") zip float("kFactor")).to[NestedPath]

  val cSingleField: ConfigDescriptor[SingleField] =
    int("kCount").to[SingleField]

  val cCoproductConfig: ConfigDescriptor[CoproductConfig] =
    (cDataItem.orElseEither(cNestedConfig)).to[CoproductConfig]
}
