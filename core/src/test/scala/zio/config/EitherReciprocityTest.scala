package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.helpers._
import zio.config.EitherReciprocityTestUtils._
import zio.test._
import zio.test.Assertion._

object EitherReciprocityTest
    extends BaseSpec(
      suite("Either reciprocity")(
        testM("coproduct should yield the same config representation on both sides of Either") {
          checkM(genNestedConfig) {
            p =>
              val lr =
                for {
                  writtenLeft <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Left(p))))
                  rereadLeft <- ZIO.fromEither(
                                 read(
                                   cCoproductConfig from ConfigSource
                                     .fromPropertyTree(writtenLeft, "test", LeafForSequence.Valid)
                                 )
                               )
                  writtenRight <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Right(p))))
                  rereadRight <- ZIO.fromEither(
                                  read(
                                    cCoproductConfig from ConfigSource
                                      .fromPropertyTree(writtenRight, "test", LeafForSequence.Valid)
                                  )
                                )
                } yield (rereadLeft.coproduct, rereadRight.coproduct) match {
                  case (Left(pl), Right(pr)) => Some(pl -> pr)
                  case _                     => None
                }

              assertM(lr)(isSome(equalTo(p -> p)))
          }
        }
      )
    )

object EitherReciprocityTestUtils {
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedPath(enterpriseAuth: EnterpriseAuth, count: Int, factor: Float)
  final case class CoproductConfig(coproduct: Either[NestedPath, NestedPath])

  private val genEnterpriseAuth =
    for {
      id    <- genId
      dburl <- genDbUrl
    } yield EnterpriseAuth(id, dburl)

  val genNestedConfig =
    for {
      auth   <- genEnterpriseAuth
      count  <- Gen.anyInt
      factor <- Gen.anyFloat
    } yield NestedPath(auth, count, factor)

  private val cIdLeft             = string("klId")(Id.apply, Id.unapply)
  private val cDbUrlLeft          = string("klDbUrl")(DbUrl.apply, DbUrl.unapply)
  private val cEnterpriseAuthLeft = (cIdLeft |@| cDbUrlLeft)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

  private val cNestedConfigLeft =
    (cEnterpriseAuthLeft |@| int("klCount") |@| float("klFactor"))(NestedPath.apply, NestedPath.unapply)

  private val cIdRight             = string("krId")(Id.apply, Id.unapply)
  private val cDbUrlRight          = string("krDbUrl")(DbUrl.apply, DbUrl.unapply)
  private val cEnterpriseAuthRight = (cIdRight |@| cDbUrlRight)(EnterpriseAuth.apply, EnterpriseAuth.unapply)

  private val cNestedConfigRight =
    (cEnterpriseAuthRight |@| int("krCount") |@| float("krFactor"))(NestedPath.apply, NestedPath.unapply)

  val cCoproductConfig =
    (cNestedConfigLeft.orElseEither(cNestedConfigRight))(CoproductConfig.apply, CoproductConfig.unapply)

}
