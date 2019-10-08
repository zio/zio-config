package zio.config

import org.scalacheck.{ Gen, Properties }
import zio.config.testsupport.TestSupport
import zio.config.Config._

object ReadWriteRoundtripTest extends Properties("Coproduct support") with TestSupport {

  private val genId    = genSymbol(1, 5).map(Id)
  private val genDbUrl = genNonEmptyString(20).map(DbUrl)
  private val genEnterpriseAuth =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)
  private val genNestedConfig =
    for {
      auth   <- genEnterpriseAuth
      count  <- genFor[Int]
      factor <- genFor[Double]
    } yield NestedConfig(auth, count, factor)
  private val genDataItem =
    for {
      oid   <- Gen.option(genId)
      count <- genFor[Int]
    } yield DataItem(oid, count)
  private val genCoproductConfig = Gen.either(genDataItem, genNestedConfig).map(CoproductConfig)

  private val cId: ConfigDescriptor[Id]       = string("kId").xmap(Id)(_.value)
  private val cDbUrl: ConfigDescriptor[DbUrl] = string("kDbUrl").xmap(DbUrl)(_.value)
  private val cEnterpriseAuth: ConfigDescriptor[EnterpriseAuth] =
    (cId |@| cDbUrl)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  private val cNestedConfig: ConfigDescriptor[NestedConfig] =
    (cEnterpriseAuth |@| int("kCount") |@| double("kFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )
  private val cId2: ConfigDescriptor[Id] = string("kId2").xmap(Id)(_.value)
  private val cDataItem: ConfigDescriptor[DataItem] =
    (cId2.optional |@| int("kDiCount"))(
      DataItem.apply,
      DataItem.unapply
    )
  private val cCoproductConfig: ConfigDescriptor[CoproductConfig] =
    (cDataItem orElseEither cNestedConfig)
      .xmap(CoproductConfig)(_.coproduct)

  property("newtype 1 roundtrip") = forAllZIO(genId) { p =>
    val p2 =
      for {
        written <- write(cId).provide(p)
        reread  <- read(cId).provide(mapSource(written))
      } yield reread

    p2.shouldBe(p)
  }

  property("newtype 2 roundtrip") = forAllZIO(genDbUrl) { p =>
    val p2 =
      for {
        written <- write(cDbUrl).provide(p)
        reread  <- read(cDbUrl).provide(mapSource(written))
      } yield reread

    p2.shouldBe(p)
  }

  property("case class 1 roundtrip") = forAllZIO(genEnterpriseAuth) { p =>
    val p2 =
      for {
        written <- write(cEnterpriseAuth).provide(p)
        reread  <- read(cEnterpriseAuth).provide(mapSource(written))
      } yield reread

    p2.shouldBe(p)
  }

  property("nested case class roundtrip") = forAllZIO(genNestedConfig) { p =>
    val p2 =
      for {
        written <- write(cNestedConfig).provide(p)
        reread  <- read(cNestedConfig).provide(mapSource(written))
      } yield reread

    p2.shouldBe(p)
  }

  property("coproduct roundtrip") = forAllZIO(genCoproductConfig) { p =>
    val p2 =
      for {
        written <- write(cCoproductConfig).provide(p)
        reread  <- read(cCoproductConfig).provide(mapSource(written))
      } yield reread

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
