package zio.config

import zio.config.ConfigDescriptor._
import zio.config.ProductBuilderTestUtils._
import zio.test.Assertion._
import zio.test._
import zio.{Has, Random, ZIO}

object ProductBuilderTest extends BaseSpec {

  val spec: Spec[Has[TestConfig] with Has[Random], TestFailure[String], TestSuccess] =
    suite("ProductBuilder")(
      test("combine 22 for case class") {
        check(genS22) { p =>
          val p2 =
            for {
              written <- ZIO.fromEither(write(cS22, p))
              reread  <-
                read(
                  cS22 from ConfigSource
                    .fromMultiMap(written.flattenString("."), "test")
                )
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(p))
        }
      },
      test("combine 22 for tupled") {
        check(genS22) { p =>
          val conv  = implicitly[TupleConversion[
            S22,
            (
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            )
          ]]
          val tuple = conv.to(p)

          val p2 =
            for {
              written <- ZIO.fromEither(write(cS22Tupled, tuple))
              reread  <-
                read(
                  cS22Tupled from ConfigSource
                    .fromMultiMap(written.flattenString("."), "test")
                )
                  .mapError(_.getMessage)
            } yield reread

          assertM(p2)(equalTo(tuple))
        }
      }
    )
}

object ProductBuilderTestUtils {
  final case class S22(
    s0: Int,
    s1: Int,
    s2: Int,
    s3: Int,
    s4: Int,
    s5: Int,
    s6: Int,
    s7: Int,
    s8: Int,
    s9: Int,
    s10: Int,
    s11: Int,
    s12: Int,
    s13: Int,
    s14: Int,
    s15: Int,
    s16: Int,
    s17: Int,
    s18: Int,
    s19: Int,
    s20: Int,
    s21: Int
  )

  private val cId0  = int(s"kId0")
  private val cId1  = int(s"kId1")
  private val cId2  = int(s"kId2")
  private val cId3  = int(s"kId3")
  private val cId4  = int(s"kId4")
  private val cId5  = int(s"kId5")
  private val cId6  = int(s"kId6")
  private val cId7  = int(s"kId7")
  private val cId8  = int(s"kId8")
  private val cId9  = int(s"kId9")
  private val cId10 = int(s"kId10")
  private val cId11 = int(s"kId11")
  private val cId12 = int(s"kId12")
  private val cId13 = int(s"kId13")
  private val cId14 = int(s"kId14")
  private val cId15 = int(s"kId15")
  private val cId16 = int(s"kId16")
  private val cId17 = int(s"kId17")
  private val cId18 = int(s"kId18")
  private val cId19 = int(s"kId19")
  private val cId20 = int(s"kId20")
  private val cId21 = int(s"kId21")

  val cS22: ConfigDescriptor[S22] =
    (cId0 |@| cId1 |@| cId2 |@| cId3 |@| cId4 |@| cId5 |@| cId6 |@| cId7 |@| cId8 |@| cId9 |@| cId10 |@| cId11 |@| cId12 |@| cId13 |@| cId14 |@| cId15 |@| cId16 |@| cId17 |@| cId18 |@| cId19 |@| cId20 |@| cId21)
      .to[S22]

  val cS22Tupled: ConfigDescriptor[
    (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
  ] =
    (cId0 |@| cId1 |@| cId2 |@| cId3 |@| cId4 |@| cId5 |@| cId6 |@| cId7 |@| cId8 |@| cId9 |@| cId10 |@| cId11 |@| cId12 |@| cId13 |@| cId14 |@| cId15 |@| cId16 |@| cId17 |@| cId18 |@| cId19 |@| cId20 |@| cId21).tupled

  val genS22: Gen[Has[Random], S22] =
    for {
      s0  <- Gen.int
      s1  <- Gen.int
      s2  <- Gen.int
      s3  <- Gen.int
      s4  <- Gen.int
      s5  <- Gen.int
      s6  <- Gen.int
      s7  <- Gen.int
      s8  <- Gen.int
      s9  <- Gen.int
      s10 <- Gen.int
      s11 <- Gen.int
      s12 <- Gen.int
      s13 <- Gen.int
      s14 <- Gen.int
      s15 <- Gen.int
      s16 <- Gen.int
      s17 <- Gen.int
      s18 <- Gen.int
      s19 <- Gen.int
      s20 <- Gen.int
      s21 <- Gen.int
    } yield S22(
      s0,
      s1,
      s2,
      s3,
      s4,
      s5,
      s6,
      s7,
      s8,
      s9,
      s10,
      s11,
      s12,
      s13,
      s14,
      s15,
      s16,
      s17,
      s18,
      s19,
      s20,
      s21
    )
}
