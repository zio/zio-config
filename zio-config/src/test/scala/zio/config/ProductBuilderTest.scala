package zio.config

import org.scalacheck.Properties
import zio.config.Config.int
import zio.config.testsupport.TestSupport

object ProductBuilderTest extends Properties("ProductBuilder") with TestSupport {

  private val cId0: ConfigDescriptor[Int]  = int(s"kId0")
  private val cId1: ConfigDescriptor[Int]  = int(s"kId1")
  private val cId2: ConfigDescriptor[Int]  = int(s"kId2")
  private val cId3: ConfigDescriptor[Int]  = int(s"kId3")
  private val cId4: ConfigDescriptor[Int]  = int(s"kId4")
  private val cId5: ConfigDescriptor[Int]  = int(s"kId5")
  private val cId6: ConfigDescriptor[Int]  = int(s"kId6")
  private val cId7: ConfigDescriptor[Int]  = int(s"kId7")
  private val cId8: ConfigDescriptor[Int]  = int(s"kId8")
  private val cId9: ConfigDescriptor[Int]  = int(s"kId9")
  private val cId10: ConfigDescriptor[Int] = int(s"kId10")
  private val cId11: ConfigDescriptor[Int] = int(s"kId11")
  private val cId12: ConfigDescriptor[Int] = int(s"kId12")
  private val cId13: ConfigDescriptor[Int] = int(s"kId13")
  private val cId14: ConfigDescriptor[Int] = int(s"kId14")
  private val cId15: ConfigDescriptor[Int] = int(s"kId15")
  private val cId16: ConfigDescriptor[Int] = int(s"kId16")
  private val cId17: ConfigDescriptor[Int] = int(s"kId17")
  private val cId18: ConfigDescriptor[Int] = int(s"kId18")
  private val cId19: ConfigDescriptor[Int] = int(s"kId19")
  private val cId20: ConfigDescriptor[Int] = int(s"kId20")
  private val cId21: ConfigDescriptor[Int] = int(s"kId21")

  final case class S22(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int, s17: Int, s18: Int, s19: Int, s20: Int, s21: Int)

  private val cS22: ConfigDescriptor[S22] =
    (cId0 |@| cId1 |@| cId2 |@| cId3 |@| cId4 |@| cId5 |@| cId6 |@| cId7 |@| cId8 |@| cId9 |@| cId10 |@| cId11 |@| cId12 |@| cId13 |@| cId14 |@| cId15 |@| cId16 |@| cId17 |@| cId18 |@| cId19 |@| cId20 |@| cId21)(
      S22.apply,
      S22.unapply
    )

  private val genS22 =
    for {
      s0  <- genFor[Int]
      s1  <- genFor[Int]
      s2  <- genFor[Int]
      s3  <- genFor[Int]
      s4  <- genFor[Int]
      s5  <- genFor[Int]
      s6  <- genFor[Int]
      s7  <- genFor[Int]
      s8  <- genFor[Int]
      s9  <- genFor[Int]
      s10 <- genFor[Int]
      s11 <- genFor[Int]
      s12 <- genFor[Int]
      s13 <- genFor[Int]
      s14 <- genFor[Int]
      s15 <- genFor[Int]
      s16 <- genFor[Int]
      s17 <- genFor[Int]
      s18 <- genFor[Int]
      s19 <- genFor[Int]
      s20 <- genFor[Int]
      s21 <- genFor[Int]
    } yield S22(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21)

  // This tests all the lower arities as well, since |@| is implemented in terms of the lower arities
  property("combine 22") = forAllZIO(genS22) { p =>
    val p2 =
      for {
        written <- write(cS22).run.provide(p)
        reread  <- read(cS22).provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

}
