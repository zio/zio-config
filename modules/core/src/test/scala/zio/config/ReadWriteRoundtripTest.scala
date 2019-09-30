package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.testsupport.TestSupport

object ReadWriteRoundtripTest extends Properties("Coproduct support") with TestSupport {

  val genId    = genSymbol(1, 5).map(Id)
  val genDbUrl = genNonEmptyString(20).map(DbUrl)
  val genEnterpriseAuth =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)
  val genNestedConfig =
    for {
      auth   <- genEnterpriseAuth
      count  <- genFor[Int]
      factor <- genFor[Double]
    } yield NestedConfig(auth, count, factor)
  val genDataItem =
    for {
      oid   <- Gen.option(genId)
      count <- genFor[Int]
    } yield DataItem(oid, count)
  val genCoproductConfig = Gen.either(genDataItem, genNestedConfig).map(CoproductConfig)

  val cId: Config[Id]       = string("kId").xmap(Id)(_.value)
  val cDbUrl: Config[DbUrl] = string("kDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuth: Config[EnterpriseAuth] =
    (cId <*> cDbUrl)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val cNestedConfig: Config[NestedConfig] =
    (cEnterpriseAuth <*> int("kCount") <*> double("kFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )
  val cId2: Config[Id] = string("kId2").xmap(Id)(_.value)
  val cDataItem: Config[DataItem] =
    (opt(cId2) <*> int("kDiCount"))(
      DataItem.apply,
      DataItem.unapply
    )
  val cCoproductConfig: Config[CoproductConfig] =
    (cDataItem or cNestedConfig)
      .xmap(CoproductConfig)(_.coproduct)

  property("newtype 1 roundtrip") = forAllZIO(genId) { p =>
    val p2 =
      for {
        written <- write(cId).run.provide(p)
        reread  <- read(cId).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("newtype 2 roundtrip") = forAllZIO(genDbUrl) { p =>
    val p2 =
      for {
        written <- write(cDbUrl).run.provide(p)
        reread  <- read(cDbUrl).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("case class 1 roundtrip") = forAllZIO(genEnterpriseAuth) { p =>
    val p2 =
      for {
        written <- write(cEnterpriseAuth).run.provide(p)
        reread  <- read(cEnterpriseAuth).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("nested case class roundtrip") = forAllZIO(genNestedConfig) { p =>
    val p2 =
      for {
        written <- write(cNestedConfig).run.provide(p)
        reread  <- read(cNestedConfig).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("coproduct roundtrip") = forAllZIO(genCoproductConfig) { p =>
    val p2 =
      for {
        written <- write(cCoproductConfig).run.provide(p)
        reread  <- read(cCoproductConfig).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  ////

  final case class Id(value: String)    extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedConfig(enterpriseAuth: EnterpriseAuth, count: Int, factor: Double)
  final case class DataItem(oid: Option[Id], count: Int)
  final case class CoproductConfig(coproduct: Either[DataItem, NestedConfig])

}
