package zio.config

import java.net.URI
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import zio.config.PropertyType.PropertyReadError

import scala.concurrent.duration.Duration
import scala.util.{ Failure, Success, Try }

trait PropertyType[A] {
  def read(propertyValue: String): Either[PropertyReadError, A]
  def write(a: A): String
}

object PropertyType {
  final case class PropertyReadError(value: String, typeInfo: String)

  case object StringType extends PropertyType[String] {
    def read(value: String): Either[PropertyReadError, String] = Right(value)
    def write(a: String): String                               = a
  }

  case object BooleanType extends PropertyType[Boolean] {
    def read(value: String): Either[PropertyReadError, Boolean] =
      attempt(value.toBoolean, _ => PropertyReadError(value, "boolean"))
    def write(value: Boolean): String = value.toString
  }

  case object ByteType extends PropertyType[Byte] {
    def read(value: String): Either[PropertyReadError, Byte] =
      attempt(value.toByte, _ => PropertyReadError(value, "byte"))
    def write(value: Byte): String = value.toString
  }

  case object ShortType extends PropertyType[Short] {
    def read(value: String): Either[PropertyReadError, Short] =
      attempt(value.toShort, _ => PropertyReadError(value, "short"))
    def write(value: Short): String = value.toString
  }

  case object IntType extends PropertyType[Int] {
    def read(value: String): Either[PropertyReadError, Int] =
      attempt(value.toInt, _ => PropertyReadError(value, "int"))
    def write(value: Int): String = value.toString
  }

  case object LongType extends PropertyType[Long] {
    def read(value: String): Either[PropertyReadError, Long] =
      attempt(value.toLong, _ => PropertyReadError(value, "long"))
    def write(value: Long): String = value.toString
  }

  case object BigIntType extends PropertyType[BigInt] {
    def read(value: String): Either[PropertyReadError, BigInt] =
      attempt(BigInt(value), _ => PropertyReadError(value, "bigint"))
    def write(value: BigInt): String = value.toString
  }

  case object FloatType extends PropertyType[Float] {
    def read(value: String): Either[PropertyReadError, Float] =
      attempt(value.toFloat, _ => PropertyReadError(value, "float"))
    def write(value: Float): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def read(value: String): Either[PropertyReadError, Double] =
      attempt(value.toDouble, _ => PropertyReadError(value, "double"))
    def write(value: Double): String = value.toString
  }

  case object BigDecimalType extends PropertyType[BigDecimal] {
    def read(value: String): Either[PropertyReadError, BigDecimal] =
      attempt(BigDecimal(value), _ => PropertyReadError(value, "bigdecimal"))
    def write(value: BigDecimal): String = value.toString
  }

  case object UriType extends PropertyType[URI] {
    def read(value: String): Either[PropertyReadError, URI] =
      attempt(new URI(value), _ => PropertyReadError(value, "uri"))
    def write(value: URI): String = value.toString
  }

  case object DurationType extends PropertyType[Duration] {
    def read(value: String): Either[PropertyReadError, Duration] =
      attempt(Duration.apply(value), _ => PropertyReadError(value, "duration"))
    def write(value: Duration): String = value.toString
  }

  case object UuidType extends PropertyType[UUID] {
    def read(value: String): Either[PropertyReadError, UUID] =
      attempt(UUID.fromString(value), _ => PropertyReadError(value, "uuid"))
    def write(value: UUID): String = value.toString
  }

  case object ZioDurationType extends PropertyType[zio.duration.Duration] {
    def read(value: String): Either[PropertyReadError, zio.duration.Duration] =
      attempt(zio.duration.Duration.fromScala(Duration.apply(value)), _ => PropertyReadError(value, "duration"))
    def write(value: zio.duration.Duration): String = value.render
  }

  case object LocalDateType extends PropertyType[LocalDate] {
    def read(value: String): Either[PropertyReadError, LocalDate] =
      attempt(LocalDate.parse(value), _ => PropertyReadError(value, "localdate"))
    def write(value: LocalDate): String = value.toString
  }

  case object LocalDateTimeType extends PropertyType[LocalDateTime] {
    def read(value: String): Either[PropertyReadError, LocalDateTime] =
      attempt(LocalDateTime.parse(value), _ => PropertyReadError(value, "localdatetime"))
    def write(value: LocalDateTime): String = value.toString
  }

  case object LocalTimeType extends PropertyType[LocalTime] {
    def read(value: String): Either[PropertyReadError, LocalTime] =
      attempt(LocalTime.parse(value), _ => PropertyReadError(value, "localtime"))
    def write(value: LocalTime): String = value.toString
  }

  case object InstantType extends PropertyType[Instant] {
    def read(value: String): Either[PropertyReadError, Instant] =
      attempt(Instant.parse(value), _ => PropertyReadError(value, "instant"))
    def write(value: Instant): String = value.toString
  }

  private def attempt[A, E](a: => A, f: Throwable => E): Either[E, A] =
    Try(a) match {
      case Success(value)     => Right(value)
      case Failure(throwable) => Left(f(throwable))
    }
}
