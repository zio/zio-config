package zio.config

import zio.ZIO
import zio.config.Config._
import zio.config.ProductBuilderTestUtils._
import zio.test._
import zio.test.Assertion._

object ProductBuilderTest
    extends BaseSpec(
      suite("ProductBuilder")(
        testM("combine 22") {
          checkM(genS22) { p =>
            val p2 =
              for {
                written <- ZIO.fromEither(write(cS22, p))
                reread  <- read(cS22).provide(ConfigSource.fromMap(written.flatten(".")))
              } yield reread

            assertM(p2, equalTo(p))
          }
        }
      )
    )

object ProductBuilderTestUtils {
  final case class S22(s0: Int, s1: Int, s2: Int, s3: Int,
                       s4: Int, s5: Int, s6: Int, s7: Int,
                       s8: Int, s9: Int, s10: Int, s11: Int,
                       s12: Int, s13: Int, s14: Int, s15: Int,
                       s16: Int, s17: Int, s18: Int, s19: Int,
                       s20: Int, s21: Int)

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

  val cS22 =
    (cId0 |@| cId1 |@| cId2 |@| cId3 |@| cId4 |@| cId5 |@| cId6 |@| cId7 |@| cId8 |@| cId9 |@| cId10 |@| cId11 |@| cId12 |@| cId13 |@| cId14 |@| cId15 |@| cId16 |@| cId17 |@| cId18 |@| cId19 |@| cId20 |@| cId21)(
      S22.apply,
      S22.unapply
    )

  val genS22 =
    for {
      s0  <- Gen.anyInt
      s1  <- Gen.anyInt
      s2  <- Gen.anyInt
      s3  <- Gen.anyInt
      s4  <- Gen.anyInt
      s5  <- Gen.anyInt
      s6  <- Gen.anyInt
      s7  <- Gen.anyInt
      s8  <- Gen.anyInt
      s9  <- Gen.anyInt
      s10 <- Gen.anyInt
      s11 <- Gen.anyInt
      s12 <- Gen.anyInt
      s13 <- Gen.anyInt
      s14 <- Gen.anyInt
      s15 <- Gen.anyInt
      s16 <- Gen.anyInt
      s17 <- Gen.anyInt
      s18 <- Gen.anyInt
      s19 <- Gen.anyInt
      s20 <- Gen.anyInt
      s21 <- Gen.anyInt
    } yield S22(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21)
}
