package zio.config

import zio.config.Config._
import zio.config.helpers._
import zio.test._
import zio.test.Assertion._

object ReadWriteRoundtripTest
    extends BaseSpec(
      suite("Coproduct support")(
        testM("newtype 1 roundtrip") {
          checkM(genId) { p =>
            val cId = string("kId").xmap(Id)(_.value)

            val p2 =
              for {
                written <- write(cId).provide(p)
                reread  <- read(cId).provide(mapSource(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("newtype 2 roundtrip") {
          checkM(genDbUrl) { p =>
            val cDbUrl = string("kDbUrl").xmap(DbUrl)(_.value)

            val p2 =
              for {
                written <- write(cDbUrl).provide(p)
                reread  <- read(cDbUrl).provide(mapSource(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("case class 1 roundtrip") {
          checkM(genEnterpriseAuth) { p =>
            val cId             = string("kId").xmap(Id)(_.value)
            val cDbUrl          = string("kDbUrl").xmap(DbUrl)(_.value)
            val cEnterpriseAuth = (cId |@| cDbUrl)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

            val p2 =
              for {
                written <- write(cEnterpriseAuth).provide(p)
                reread  <- read(cEnterpriseAuth).provide(mapSource(written))
              } yield reread

            assertM(p2, equalTo(p))
          }
        },
        testM("nested case class roundtrip") {
          checkM(genNestedConfig) {
            p =>
              val cId             = string("kId").xmap(Id)(_.value)
              val cDbUrl          = string("kDbUrl").xmap(DbUrl)(_.value)
              val cEnterpriseAuth = (cId |@| cDbUrl)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

              val cNestedConfig =
                (cEnterpriseAuth |@| int("kCount") |@| float("kFactor"))(
                  NestedConfig.apply,
                  NestedConfig.unapply
                )

              val p2 =
                for {
                  written <- write(cNestedConfig).provide(p)
                  reread  <- read(cNestedConfig).provide(mapSource(written))
                } yield reread

              assertM(p2, equalTo(p))
          }
        },
        testM("coproduct roundtrip") {
          checkM(genCoproductConfig) {
            p =>
              val cId2            = string("kId2").xmap(Id)(_.value)
              val cDataItem       = (cId2.optional |@| int("kDiCount"))(DataItem.apply, DataItem.unapply)
              val cId             = string("kId").xmap(Id)(_.value)
              val cDbUrl          = string("kDbUrl").xmap(DbUrl)(_.value)
              val cEnterpriseAuth = (cId |@| cDbUrl)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

              val cNestedConfig =
                (cEnterpriseAuth |@| int("kCount") |@| float("kFactor"))(NestedConfig.apply, NestedConfig.unapply)

              val cCoproductConfig =
                (cDataItem.orElseEither(cNestedConfig)).xmap(CoproductConfig)(_.coproduct)

              val p2 =
                for {
                  written <- write(cCoproductConfig).provide(p)
                  reread  <- read(cCoproductConfig).provide(mapSource(written))
                } yield reread

              assertM(p2, equalTo(p))
          }
        }
      )
    )
