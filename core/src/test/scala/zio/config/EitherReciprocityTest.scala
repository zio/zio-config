package zio.config

import zio.ZIO
import zio.config.ConfigDescriptor._
import zio.config.EitherReciprocityTestUtils._
import zio.config.helpers._
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object EitherReciprocityTest extends BaseSpec {

  val spec: Spec[TestConfig with Random, TestFailure[Serializable], TestSuccess] =
    suite("Either reciprocity")(
      testM("coproduct should yield the same config representation on both sides of Either") {
        checkM(genNestedConfig) { p =>
          val lr =
            for {
              writtenLeft  <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Left(p))))
              rereadLeft   <- ZIO.fromEither(
                                read(
                                  cCoproductConfig from ConfigSource
                                    .fromPropertyTree(writtenLeft, "test", LeafForSequence.Valid)
                                )
                              )
              writtenRight <- ZIO.fromEither(write(cCoproductConfig, CoproductConfig(Right(p))))
              rereadRight  <- ZIO.fromEither(
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
}

object EitherReciprocityTestUtils {
  final case class EnterpriseAuth(id: Id, dburl: DbUrl)
  final case class NestedPath(enterpriseAuth: EnterpriseAuth, count: Int, factor: Float)
  final case class CoproductConfig(coproduct: Either[NestedPath, NestedPath])

  private val genEnterpriseAuth =
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

  private val cIdLeft             = string("klId").to[Id]
  private val cDbUrlLeft          = string("klDbUrl").to[DbUrl]
  private val cEnterpriseAuthLeft = (cIdLeft |@| cDbUrlLeft).to[EnterpriseAuth]

  private val cNestedConfigLeft =
    (cEnterpriseAuthLeft |@| int("klCount") |@| float("klFactor")).to[NestedPath]

  private val cIdRight             = string("krId").to[Id]
  private val cDbUrlRight          = string("krDbUrl").to[DbUrl]
  private val cEnterpriseAuthRight = (cIdRight |@| cDbUrlRight).to[EnterpriseAuth]

  private val cNestedConfigRight =
    (cEnterpriseAuthRight |@| int("krCount") |@| float("krFactor")).to[NestedPath]

  val cCoproductConfig: ConfigDescriptor[CoproductConfig] =
    (cNestedConfigLeft.orElseEither(cNestedConfigRight)).to[CoproductConfig]

}
