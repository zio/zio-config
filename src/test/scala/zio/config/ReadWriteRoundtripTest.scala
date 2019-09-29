package zio.config

import org.scalacheck.Properties
import zio.config.testsupport.TestSupport

object ReadWriteRoundtripTest extends Properties("Coproduct support") with TestSupport {

  val genId    = genSymbol(1, 5).map(Id)
  val genDbUrl = genNonEmptyString(20).map(DbUrl)
  val genEnterpriseAuth =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)
  val genNestedStuff =
    for {
      auth   <- genEnterpriseAuth
      count  <- genFor[Int]
      factor <- genFor[Double]
    } yield NestedStuff(auth, count, factor)
  val genCoproductConfig =
    for {
      id     <- genId
      nested <- genNestedStuff
      isLeft <- genBoolean
      isNone <- genBoolean
    } yield CoproductConfig(if (isLeft) Left(if (isNone) None else Some(id)) else Right(nested))

  val cId: Config[Id]       = string("kId").xmap(Id)(_.value)
  val cDbUrl: Config[DbUrl] = string("kDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuth: Config[EnterpriseAuth] =
    (cId <*> cDbUrl)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val cNestedStuff: Config[NestedStuff] =
    (cEnterpriseAuth <*> int("kCount") <*> double("kFactor"))(
      NestedStuff.apply,
      NestedStuff.unapply
    )
  val cId2: Config[Id] = string("kId2").xmap(Id)(_.value)
  val cCoproductConfig: Config[CoproductConfig] =
    (opt(cId2) or cNestedStuff)
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

  property("nested case class roundtrip") = forAllZIO(genNestedStuff) { p =>
    val p2 =
      for {
        written <- write(cNestedStuff).run.provide(p)
        reread  <- read(cNestedStuff).run.provide(mapSource(written.allConfig))
      } yield reread._2

    p2.shouldBe(p)
  }

  // TODO inhibited failing test until fixed
  //  property("coproduct roundtrip") = forAllZIO(genCoproductConfig) { p =>
  //    val p2 =
  //      for {
  //        written <- write(cCoproductConfig).run.provide(p)
  //        reread  <- read(cCoproductConfig).run.provide(mapSource(written.allConfig))
  //      } yield reread._2
  //
  //    p2.shouldBe(p)
  //  }

  ////

  final case class Id(value: String)    extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedStuff(enterpriseAuth: EnterpriseAuth, count: Int, factor: Double)
  final case class CoproductConfig(coproduct: Either[Option[Id], NestedStuff])

}
