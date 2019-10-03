package zio.config

import org.scalacheck.Properties
import zio.config.Config.int
import zio.config.testsupport.TestSupport

object ProductBuilderTest extends Properties("ProductBuilder") with TestSupport {

  val cId0: Config[Int]  = int("kId0")
  val cId1: Config[Int]  = int("kId1")
  val cId2: Config[Int]  = int("kId2")
  val cId3: Config[Int]  = int("kId3")
  val cId4: Config[Int]  = int("kId4")
  val cId5: Config[Int]  = int("kId5")
  val cId6: Config[Int]  = int("kId6")
  val cId7: Config[Int]  = int("kId7")
  val cId8: Config[Int]  = int("kId8")
  val cId9: Config[Int]  = int("kId9")
  val cId10: Config[Int] = int("kId10")
  val cId11: Config[Int] = int("kId11")
  val cId12: Config[Int] = int("kId12")
  val cId13: Config[Int] = int("kId13")
  val cId14: Config[Int] = int("kId14")
  val cId15: Config[Int] = int("kId15")
  val cId16: Config[Int] = int("kId16")
  val cId17: Config[Int] = int("kId17")
  val cId18: Config[Int] = int("kId18")
  val cId19: Config[Int] = int("kId19")
  val cId20: Config[Int] = int("kId20")
  val cId21: Config[Int] = int("kId21")

  final case class S2(s0: Int, s1: Int)
  final case class S3(s0: Int, s1: Int, s2: Int)
  final case class S4(s0: Int, s1: Int, s2: Int, s3: Int)
  final case class S5(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int)
  final case class S6(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int)
  final case class S7(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int)
  final case class S8(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int)
  final case class S9(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int)
  final case class S10(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int)
  final case class S11(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int)
  final case class S12(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int)
  final case class S13(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int)
  final case class S14(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int)
  final case class S15(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int)
  final case class S16(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int)
  final case class S17(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int)
  final case class S18(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int, s17: Int)
  final case class S19(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int, s17: Int, s18: Int)
  final case class S20(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int, s17: Int, s18: Int, s19: Int)
  final case class S21(s0: Int, s1: Int, s2: Int, s3: Int, s4: Int, s5: Int, s6: Int, s7: Int, s8: Int, s9: Int, s10: Int, s11: Int, s12: Int, s13: Int, s14: Int, s15: Int, s16: Int, s17: Int, s18: Int, s19: Int, s20: Int)

  val cS2: Config[S2] =
    (cId0 <*> cId1)(
      S2.apply,
      S2.unapply
    )

  val cS3: Config[S3] =
    (cId0 <*> cId1 <*> cId2)(
      S3.apply,
      S3.unapply
    )

  val cS4: Config[S4] =
    (cId0 <*> cId1 <*> cId2 <*> cId3)(
      S4.apply,
      S4.unapply
    )

  val cS5: Config[S5] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4)(
      S5.apply,
      S5.unapply
    )

  val cS6: Config[S6] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5)(
      S6.apply,
      S6.unapply
    )

  val cS7: Config[S7] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6)(
      S7.apply,
      S7.unapply
    )

  val cS8: Config[S8] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7)(
      S8.apply,
      S8.unapply
    )

  val cS9: Config[S9] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8)(
      S9.apply,
      S9.unapply
    )

  val cS10: Config[S10] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9)(
      S10.apply,
      S10.unapply
    )

  val cS11: Config[S11] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10)(
      S11.apply,
      S11.unapply
    )

  val cS12: Config[S12] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11)(
      S12.apply,
      S12.unapply
    )

  val cS13: Config[S13] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12)(
      S13.apply,
      S13.unapply
    )

  val cS14: Config[S14] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13)(
      S14.apply,
      S14.unapply
    )

  val cS15: Config[S15] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14)(
      S15.apply,
      S15.unapply
    )

  val cS16: Config[S16] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15)(
      S16.apply,
      S16.unapply
    )

  val cS17: Config[S17] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15 <*> cId16)(
      S17.apply,
      S17.unapply
    )

  val cS18: Config[S18] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15 <*> cId16 <*> cId17)(
      S18.apply,
      S18.unapply
    )

  val cS19: Config[S19] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15 <*> cId16 <*> cId17 <*> cId18)(
      S19.apply,
      S19.unapply
    )

  val cS20: Config[S20] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15 <*> cId16 <*> cId17 <*> cId18 <*> cId19)(
      S20.apply,
      S20.unapply
    )

  val cS21: Config[S21] =
    (cId0 <*> cId1 <*> cId2 <*> cId3 <*> cId4 <*> cId5 <*> cId6 <*> cId7 <*> cId8 <*> cId9 <*> cId10 <*> cId11 <*> cId12 <*> cId13 <*> cId14 <*> cId15 <*> cId16 <*> cId17 <*> cId18 <*> cId19 <*> cId20)(
      S21.apply,
      S21.unapply
    )

  ////

  val genS2 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
    } yield S2(s0, s1)

  val genS3 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
    } yield S3(s0, s1, s2)

  val genS4 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
    } yield S4(s0, s1, s2, s3)

  val genS5 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
    } yield S5(s0, s1, s2, s3, s4)

  val genS6 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
      s5 <- genFor[Int]
    } yield S6(s0, s1, s2, s3, s4, s5)

  val genS7 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
      s5 <- genFor[Int]
      s6 <- genFor[Int]
    } yield S7(s0, s1, s2, s3, s4, s5, s6)

  val genS8 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
      s5 <- genFor[Int]
      s6 <- genFor[Int]
      s7 <- genFor[Int]
    } yield S8(s0, s1, s2, s3, s4, s5, s6, s7)

  val genS9 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
      s5 <- genFor[Int]
      s6 <- genFor[Int]
      s7 <- genFor[Int]
      s8 <- genFor[Int]
    } yield S9(s0, s1, s2, s3, s4, s5, s6, s7, s8)

  val genS10 =
    for {
      s0 <- genFor[Int]
      s1 <- genFor[Int]
      s2 <- genFor[Int]
      s3 <- genFor[Int]
      s4 <- genFor[Int]
      s5 <- genFor[Int]
      s6 <- genFor[Int]
      s7 <- genFor[Int]
      s8 <- genFor[Int]
      s9 <- genFor[Int]
    } yield S10(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)

  val genS11 =
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
    } yield S11(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10)

  val genS12 =
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
    } yield S12(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11)

  val genS13 =
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
    } yield S13(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)

  val genS14 =
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
    } yield S14(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13)

  val genS15 =
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
    } yield S15(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)

  val genS16 =
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
    } yield S16(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)

  val genS17 =
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
    } yield S17(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16)

  val genS18 =
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
    } yield S18(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17)

  val genS19 =
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
    } yield S19(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18)

  val genS20 =
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
    } yield S20(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19)

  val genS21 =
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
    } yield S21(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)

  ////

  property("combine 2") = forAllZIO(genS2) { p =>
    val p2 =
      for {
        written <- write(cS2).run.provide(p)
        reread  <- read(cS2).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 3") = forAllZIO(genS3) { p =>
    val p2 =
      for {
        written <- write(cS3).run.provide(p)
        reread  <- read(cS3).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 4") = forAllZIO(genS4) { p =>
    val p2 =
      for {
        written <- write(cS4).run.provide(p)
        reread  <- read(cS4).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 5") = forAllZIO(genS5) { p =>
    val p2 =
      for {
        written <- write(cS5).run.provide(p)
        reread  <- read(cS5).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 6") = forAllZIO(genS6) { p =>
    val p2 =
      for {
        written <- write(cS6).run.provide(p)
        reread  <- read(cS6).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 7") = forAllZIO(genS7) { p =>
    val p2 =
      for {
        written <- write(cS7).run.provide(p)
        reread  <- read(cS7).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 8") = forAllZIO(genS8) { p =>
    val p2 =
      for {
        written <- write(cS8).run.provide(p)
        reread  <- read(cS8).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 9") = forAllZIO(genS9) { p =>
    val p2 =
      for {
        written <- write(cS9).run.provide(p)
        reread  <- read(cS9).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 10") = forAllZIO(genS10) { p =>
    val p2 =
      for {
        written <- write(cS10).run.provide(p)
        reread  <- read(cS10).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 11") = forAllZIO(genS11) { p =>
    val p2 =
      for {
        written <- write(cS11).run.provide(p)
        reread  <- read(cS11).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 12") = forAllZIO(genS12) { p =>
    val p2 =
      for {
        written <- write(cS12).run.provide(p)
        reread  <- read(cS12).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 13") = forAllZIO(genS13) { p =>
    val p2 =
      for {
        written <- write(cS13).run.provide(p)
        reread  <- read(cS13).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 14") = forAllZIO(genS14) { p =>
    val p2 =
      for {
        written <- write(cS14).run.provide(p)
        reread  <- read(cS14).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 15") = forAllZIO(genS15) { p =>
    val p2 =
      for {
        written <- write(cS15).run.provide(p)
        reread  <- read(cS15).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 16") = forAllZIO(genS16) { p =>
    val p2 =
      for {
        written <- write(cS16).run.provide(p)
        reread  <- read(cS16).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 17") = forAllZIO(genS17) { p =>
    val p2 =
      for {
        written <- write(cS17).run.provide(p)
        reread  <- read(cS17).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 18") = forAllZIO(genS18) { p =>
    val p2 =
      for {
        written <- write(cS18).run.provide(p)
        reread  <- read(cS18).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 19") = forAllZIO(genS19) { p =>
    val p2 =
      for {
        written <- write(cS19).run.provide(p)
        reread  <- read(cS19).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 20") = forAllZIO(genS20) { p =>
    val p2 =
      for {
        written <- write(cS20).run.provide(p)
        reread  <- read(cS20).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

  property("combine 21") = forAllZIO(genS21) { p =>
    val p2 =
      for {
        written <- write(cS21).run.provide(p)
        reread  <- read(cS21).run.provide(mapSource(written))
      } yield reread._2

    p2.shouldBe(p)
  }

}
