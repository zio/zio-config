package zio.config.gen

import zio.test.magnolia.DeriveGen
import zio.test.{Gen, Sized}

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime}
import java.util.UUID
import scala.concurrent.duration.Duration

/**
 * DeriveGenInstances gives instance for DeriveGen for all the types
 * that zio-config supports. It will also make sure the randomness for these
 * types is constrained to produce an almost valid config for the users.
 */
trait DeriveGenInstances {
  // Implicits to make sure there randomness is not going too far into its limits

  implicit def deriveGenBigDecimal: DeriveGen[BigDecimal] =
    new DeriveGen[BigDecimal] {
      override def derive: Gen[Sized, BigDecimal] =
        Gen.bigDecimal(0, 9999999)
    }

  implicit def deriveGenBigInt: DeriveGen[BigInt] =
    new DeriveGen[BigInt] {
      override def derive: Gen[Sized, BigInt] =
        Gen.bigInt(0, 9999999)
    }

  implicit def deriveGenByte: DeriveGen[Byte] =
    new DeriveGen[Byte] {
      override def derive: Gen[Sized, Byte] =
        Gen.byte(0, 20)
    }

  implicit def deriveGenDouble: DeriveGen[Double] =
    new DeriveGen[Double] {
      override def derive: Gen[Sized, Double] =
        Gen.double(0, 9999)
    }

  implicit def deriveGenDuration: DeriveGen[Duration] =
    new DeriveGen[Duration] {
      override def derive: Gen[Sized, Duration] =
        Gen.int(1, 10).flatMap(int => Gen.const(Duration.apply(s"${int} seconds")))
    }

  implicit def deriveGenFile: DeriveGen[File] =
    new DeriveGen[File] {
      override def derive: Gen[Sized, File] =
        Gen.alphaNumericStringBounded(3, 10).flatMap(r => Gen.const(new File(s"/Users/abc/${r}")))
    }

  implicit def deriveGenFloat: DeriveGen[Float] =
    new DeriveGen[Float] {
      override def derive: Gen[Sized, Float] =
        Gen.double(0, 9999).map(_.toFloat)
    }

  implicit def deriveGenInstant: DeriveGen[Instant] =
    new DeriveGen[Instant] {
      override def derive: Gen[Sized, Instant] =
        Gen
          .int(1000, 2000)
          .flatMap(prev => Gen.instant(Instant.now().minusSeconds(prev.toLong), Instant.now()))
    }

  implicit def deriveGenInt: DeriveGen[Int] =
    new DeriveGen[Int] {
      override def derive: Gen[Sized, Int] =
        Gen.int(0, 9999)
    }

  implicit def deriveGenJavaFilePath: DeriveGen[java.nio.file.Path] =
    new DeriveGen[java.nio.file.Path] {
      override def derive: Gen[Sized, java.nio.file.Path] =
        DeriveGen[File].map(_.toPath())
    }

  implicit def deriveGenList[A: DeriveGen]: DeriveGen[List[A]] = new DeriveGen[List[A]] {
    override def derive: Gen[Sized, List[A]] =
      Gen.int(1, 6).flatMap(n => Gen.listOfN(n)(DeriveGen[A]))
  }

  implicit def deriveGenLocalDate: DeriveGen[LocalDate] =
    new DeriveGen[LocalDate] {
      override def derive: Gen[Sized, LocalDate] =
        Gen
          .int(5, 25)
          .flatMap(prev =>
            Gen.localDateTime(LocalDateTime.now().minusHours(prev.toLong), LocalDateTime.now()).map(_.toLocalDate())
          )
    }

  implicit def deriveGenLong: DeriveGen[Long] =
    new DeriveGen[Long] {
      override def derive: Gen[Sized, Long] =
        Gen.long(10000, 999999999)
    }

  implicit def deriveGenMap[A: DeriveGen, B: DeriveGen]: DeriveGen[Map[A, B]] = new DeriveGen[Map[A, B]] {
    override def derive: Gen[Sized, Map[A, B]] =
      Gen.int(1, 6).flatMap(n => Gen.mapOfN(n)(DeriveGen[A], DeriveGen[B]))
  }

  implicit def deriveGenShort: DeriveGen[Short] =
    new DeriveGen[Short] {
      override def derive: Gen[Sized, Short] =
        Gen.short(1, 10)
    }

  implicit def deriveGenString: DeriveGen[String] =
    new DeriveGen[String] {
      override def derive: Gen[Sized, String] =
        Gen.alphaNumericStringBounded(5, 25)
    }

  implicit def deriveGenURI: DeriveGen[URI] =
    new DeriveGen[URI] {
      override def derive: Gen[Sized, URI] =
        Gen.alphaNumericStringBounded(4, 10).map(r => new URI(r))
    }

  implicit def deriveGenUUID: DeriveGen[UUID] =
    new DeriveGen[UUID] {
      override def derive: Gen[Sized, UUID] =
        Gen.const(java.util.UUID.randomUUID())
    }

  implicit def deriveGenURL: DeriveGen[URL] =
    new DeriveGen[URL] {
      override def derive: Gen[Sized, URL] =
        Gen
          .oneOf(Gen.const("abc"), Gen.const("def"))
          .flatMap(r =>
            Gen
              .oneOf(Gen.const("http"), Gen.const("https"))
              .map(prefix => new URL(s"${prefix}://${r}"))
          )
    }

  implicit def deriveGenZioDuration: DeriveGen[java.time.Duration] =
    new DeriveGen[java.time.Duration] {
      override def derive: Gen[Sized, java.time.Duration] =
        Gen.finiteDuration
    }
}
