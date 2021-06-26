package zio.config.magnolia

import zio.config._
import zio.duration.Duration
import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}

trait DeriveConfigDescriptor { self =>
  def mapClassName(name: String): String =
    name

  def mapFieldName(name: String): String =
    name

  import Descriptor.SealedTraitStrategy, SealedTraitStrategy._

  def sealedTraitStrategy: SealedTraitStrategy =
    wrapSubClassName && ignoreSealedTraitName

  import zio.config.ConfigDescriptor._

 given Descriptor[String] = Descriptor(string)
 given Descriptor[Boolean] = Descriptor(boolean)
 given Descriptor[Byte] = Descriptor(byte)
 given Descriptor[Short] = Descriptor(short)
 given Descriptor[Int] = Descriptor(int)
 given Descriptor[Long] = Descriptor(long)
 given Descriptor[BigInt] = Descriptor(bigInt)
 given Descriptor[Float] = Descriptor(float)
 given Descriptor[Double] = Descriptor(double)
 given Descriptor[BigDecimal] = Descriptor(bigDecimal)
 given Descriptor[URI] = Descriptor(uri)
 given Descriptor[URL] = Descriptor(url)
 given Descriptor[ScalaDuration] = Descriptor(duration)
 given Descriptor[Duration] = Descriptor(zioDuration)
 given Descriptor[UUID] = Descriptor(uuid)
 given Descriptor[LocalDate] = Descriptor(localDate)
 given Descriptor[LocalTime] = Descriptor(localTime)
 given Descriptor[LocalDateTime] = Descriptor(localDateTime)
 given Descriptor[Instant] = Descriptor(instant)
 given Descriptor[File] = Descriptor(file)
 given Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  implicit def implicitListDesc[A: Descriptor]: Descriptor[List[A]] =
    Descriptor(listDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitSetDesc[A: Descriptor]: Descriptor[Set[A]] =
    Descriptor(setDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitMapDesc[K, A: Descriptor]: Descriptor[Map[String, A]] =
    Descriptor(mapDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitEitherDesc[A: Descriptor, B: Descriptor]: Descriptor[Either[A, B]] =
    Descriptor(implicitly[Descriptor[A]].desc.orElseEither(implicitly[Descriptor[B]].desc))

  implicit def implicitOptionDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(implicitly[Descriptor[A]].desc.optional)

  def listDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    list(desc)

  def setDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
    set(desc)

  def mapDesc[A](
    desc: ConfigDescriptor[A]
  ): ConfigDescriptor[Map[String, A]] =
    map(desc)

  implicit def eitherDesc[A, B](
    left: ConfigDescriptor[A],
    right: ConfigDescriptor[B]
  ): ConfigDescriptor[Either[A, B]] =
    left.orElseEither(right)

  def descriptor[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    config.desc
}

object DeriveConfigDescriptor extends DeriveConfigDescriptor {
  @deprecated("Use zio.config.magnolia.Descriptor directly", since = "1.0.1")
  val Descriptor = zio.config.magnolia.Descriptor

  @deprecated("Use zio.config.magnolia.Descriptor[T] directly", since = "1.0.1")
  type Descriptor[T] = zio.config.magnolia.Descriptor[T]
}
