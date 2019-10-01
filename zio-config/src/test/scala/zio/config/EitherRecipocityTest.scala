package zio.config

import org.scalacheck.Properties
import zio.config.testsupport.TestSupport

object EitherRecipocityTest extends Properties("Reciprocity") with TestSupport {

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

  private val cIdLeft: Config[Id]       = string("klId").xmap(Id)(_.value)
  private val cDbUrlLeft: Config[DbUrl] = string("klDbUrl").xmap(DbUrl)(_.value)
  private val cEnterpriseAuthLeft: Config[EnterpriseAuth] =
    (cIdLeft <*> cDbUrlLeft)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  private val cNestedConfigLeft: Config[NestedConfig] =
    (cEnterpriseAuthLeft <*> int("klCount") <*> double("klFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  private val cIdRight: Config[Id]       = string("krId").xmap(Id)(_.value)
  private val cDbUrlRight: Config[DbUrl] = string("krDbUrl").xmap(DbUrl)(_.value)
  private val cEnterpriseAuthRight: Config[EnterpriseAuth] =
    (cIdRight <*> cDbUrlRight)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  private val cNestedConfigRight: Config[NestedConfig] =
    (cEnterpriseAuthRight <*> int("krCount") <*> double("krFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  private val cCoproductConfig: Config[CoproductConfig] =
    (cNestedConfigLeft or cNestedConfigRight)
      .xmap(CoproductConfig)(_.coproduct)

  property("coproduct should yield same config representation on both sides of Either") = forAllZIO(genNestedConfig) {
    p =>
      val lr =
        for {
          writtenLeft  <- write(cCoproductConfig).provide(CoproductConfig(Left(p)))
          rereadLeft   <- read(cCoproductConfig).provide(mapSource(writtenLeft))
          writtenRight <- write(cCoproductConfig).provide(CoproductConfig(Right(p)))
          rereadRight  <- read(cCoproductConfig).provide(mapSource(writtenRight))
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
