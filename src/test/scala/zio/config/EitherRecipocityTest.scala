package zio.config

import org.scalacheck.Properties
import zio.config.testsupport.TestSupport

object EitherRecipocityTest extends Properties("Reciprocity") with TestSupport {

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

  val cIdLeft: Config[Id]       = string("klId").xmap(Id)(_.value)
  val cDbUrlLeft: Config[DbUrl] = string("klDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuthLeft: Config[EnterpriseAuth] =
    (cIdLeft <*> cDbUrlLeft)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val cNestedConfigLeft: Config[NestedConfig] =
    (cEnterpriseAuthLeft <*> int("klCount") <*> double("klFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  val cIdRight: Config[Id]       = string("krId").xmap(Id)(_.value)
  val cDbUrlRight: Config[DbUrl] = string("krDbUrl").xmap(DbUrl)(_.value)
  val cEnterpriseAuthRight: Config[EnterpriseAuth] =
    (cIdRight <*> cDbUrlRight)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  val cNestedConfigRight: Config[NestedConfig] =
    (cEnterpriseAuthRight <*> int("krCount") <*> double("krFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  val cCoproductConfig: Config[CoproductConfig] =
    (cNestedConfigLeft or cNestedConfigRight)
      .xmap(CoproductConfig)(_.coproduct)

  property("coproduct should yield same config representation on both sides of Either") = forAllZIO(genNestedConfig) {
    p =>
      val lr =
        for {
          writtenLeft  <- write(cCoproductConfig).run.provide(CoproductConfig(Left(p)))
          rereadLeft   <- read(cCoproductConfig).run.provide(mapSource(writtenLeft.allConfig))
          writtenRight <- write(cCoproductConfig).run.provide(CoproductConfig(Right(p)))
          rereadRight  <- read(cCoproductConfig).run.provide(mapSource(writtenRight.allConfig))
        } yield {
          (rereadLeft._2.coproduct, rereadRight._2.coproduct) match {
            case (Left(pl), Right(pr)) => (Some(pl), Some(pr))
            case _                     => (None, None)
          }
        }

      lr.shouldBe((Some(p), Some(p)))
  }

  ////

  final case class Id(value: String)    extends AnyVal
  final case class DbUrl(value: String) extends AnyVal
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedConfig(enterpriseAuth: EnterpriseAuth, count: Int, factor: Double)
  final case class CoproductConfig(coproduct: Either[NestedConfig, NestedConfig])

}
