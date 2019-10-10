package zio.config

import org.scalacheck.Properties
import zio.ZIO
import zio.config.testsupport.TestSupport

object EitherRecipocityTest extends Properties("Reciprocity") with TestSupport {

  import Config._

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

  private val cIdLeft: ConfigDescriptor[Id]       = string("klId").xmap(Id)(_.value)
  private val cDbUrlLeft: ConfigDescriptor[DbUrl] = string("klDbUrl").xmap(DbUrl)(_.value)
  private val cEnterpriseAuthLeft: ConfigDescriptor[EnterpriseAuth] =
    (cIdLeft |@| cDbUrlLeft)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  private val cNestedConfigLeft: ConfigDescriptor[NestedConfig] =
    (cEnterpriseAuthLeft |@| int("klCount") |@| double("klFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  private val cIdRight: ConfigDescriptor[Id]       = string("krId").xmap(Id)(_.value)
  private val cDbUrlRight: ConfigDescriptor[DbUrl] = string("krDbUrl").xmap(DbUrl)(_.value)
  private val cEnterpriseAuthRight: ConfigDescriptor[EnterpriseAuth] =
    (cIdRight |@| cDbUrlRight)(
      EnterpriseAuth.apply,
      EnterpriseAuth.unapply
    )
  private val cNestedConfigRight: ConfigDescriptor[NestedConfig] =
    (cEnterpriseAuthRight |@| int("krCount") |@| double("krFactor"))(
      NestedConfig.apply,
      NestedConfig.unapply
    )

  private val cCoproductConfig: ConfigDescriptor[CoproductConfig] =
    (cNestedConfigLeft orElseEither cNestedConfigRight)
      .xmap(CoproductConfig)(_.coproduct)

  property("coproduct should yield same config representation on both sides of Either") = forAllZIO(genNestedConfig) {
    p =>
      val lr =
        for {
          writtenLeft  <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Left(p))))
          rereadLeft   <- read(cCoproductConfig).provide(ConfigSource.fromMap(writtenLeft))
          writtenRight <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Right(p))))
          rereadRight  <- read(cCoproductConfig).provide(ConfigSource.fromMap(writtenRight))
        } yield {
          (rereadLeft.coproduct, rereadRight.coproduct) match {
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
