package zio.config

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import zio.config.PropertyType.PropertyReadError

import scala.concurrent.duration.Duration
import scala.util.{ Failure, Success, Try }

trait PropertyType[V, A] {
  def read(propertyValue: V): Either[PropertyReadError[V], A]
  def write(a: A): V
}

object PropertyType {
  final case class PropertyReadError[V](value: V, typeInfo: String)

  case object StringType extends PropertyType[String, String] {
    def read(value: String): Either[PropertyReadError[String], String] = Right(value)
    def write(a: String): String                                       = a
  }

  case object BooleanType extends PropertyType[String, Boolean] {
    def read(value: String): Either[PropertyReadError[String], Boolean] =
      value.toLowerCase match {
        case "true" | "on" | "1"   => Right(true)
        case "false" | "off" | "0" => Right(false)
        case _                     => Left(PropertyReadError(value, "boolean"))
      }
    def write(value: Boolean): String = value.toString
  }

  case object ByteType extends PropertyType[String, Byte] {
    def read(value: String): Either[PropertyReadError[String], Byte] =
      attempt(value.toByte, _ => PropertyReadError(value, "byte"))
    def write(value: Byte): String = value.toString
  }

  case object ShortType extends PropertyType[String, Short] {
    def read(value: String): Either[PropertyReadError[String], Short] =
      attempt(value.toShort, _ => PropertyReadError(value, "short"))
    def write(value: Short): String = value.toString
  }

  case object IntType extends PropertyType[String, Int] {
    def read(value: String): Either[PropertyReadError[String], Int] =
      attempt(value.toInt, _ => PropertyReadError(value, "int"))
    def write(value: Int): String = value.toString
  }

  case object LongType extends PropertyType[String, Long] {
    def read(value: String): Either[PropertyReadError[String], Long] =
      attempt(value.toLong, _ => PropertyReadError(value, "long"))
    def write(value: Long): String = value.toString
  }

  case object BigIntType extends PropertyType[String, BigInt] {
    def read(value: String): Either[PropertyReadError[String], BigInt] =
      attempt(BigInt(value), _ => PropertyReadError(value, "bigint"))
    def write(value: BigInt): String = value.toString
  }

  case object FloatType extends PropertyType[String, Float] {
    def read(value: String): Either[PropertyReadError[String], Float] =
      attempt(value.toFloat, _ => PropertyReadError(value, "float"))
    def write(value: Float): String = value.toString
  }

  case object DoubleType extends PropertyType[String, Double] {
    def read(value: String): Either[PropertyReadError[String], Double] =
      attempt(value.toDouble, _ => PropertyReadError(value, "double"))
    def write(value: Double): String = value.toString
  }

  case object BigDecimalType extends PropertyType[String, BigDecimal] {
    def read(value: String): Either[PropertyReadError[String], BigDecimal] =
      attempt(BigDecimal(value), _ => PropertyReadError(value, "bigdecimal"))
    def write(value: BigDecimal): String = value.toString
  }

  case object UriType extends PropertyType[String, URI] {
    def read(value: String): Either[PropertyReadError[String], URI] =
      attempt(new URI(value), _ => PropertyReadError(value, "uri"))
    def write(value: URI): String = value.toString
  }

  case object DurationType extends PropertyType[String, Duration] {
    def read(value: String): Either[PropertyReadError[String], Duration] =
      attempt(Duration.apply(value), _ => PropertyReadError(value, "duration"))
    def write(value: Duration): String = value.toString
  }

  case object UuidType extends PropertyType[String, UUID] {
    def read(value: String): Either[PropertyReadError[String], UUID] =
      attempt(UUID.fromString(value), _ => PropertyReadError(value, "uuid"))
    def write(value: UUID): String = value.toString
  }

  case object ZioDurationType extends PropertyType[String, zio.duration.Duration] {
    def read(value: String): Either[PropertyReadError[String], zio.duration.Duration] =
      attempt(zio.duration.Duration.fromScala(Duration.apply(value)), _ => PropertyReadError(value, "duration"))
    def write(value: zio.duration.Duration): String = value.render
  }

  case object LocalDateType extends PropertyType[String, LocalDate] {
    def read(value: String): Either[PropertyReadError[String], LocalDate] =
      attempt(LocalDate.parse(value), _ => PropertyReadError(value, "localdate"))
    def write(value: LocalDate): String = value.toString
  }

  case object LocalDateTimeType extends PropertyType[String, LocalDateTime] {
    def read(value: String): Either[PropertyReadError[String], LocalDateTime] =
      attempt(LocalDateTime.parse(value), _ => PropertyReadError(value, "localdatetime"))
    def write(value: LocalDateTime): String = value.toString
  }

  case object LocalTimeType extends PropertyType[String, LocalTime] {
    def read(value: String): Either[PropertyReadError[String], LocalTime] =
      attempt(LocalTime.parse(value), _ => PropertyReadError(value, "localtime"))
    def write(value: LocalTime): String = value.toString
  }

  case object InstantType extends PropertyType[String, Instant] {
    def read(value: String): Either[PropertyReadError[String], Instant] =
      attempt(Instant.parse(value), _ => PropertyReadError(value, "instant"))
    def write(value: Instant): String = value.toString
  }

  case object FileType extends PropertyType[String, File] {
    def read(value: String): Either[PropertyReadError[String], File] =
      attempt(new File(value), _ => PropertyReadError(value, "file"))

    def write(value: File): String = value.toString
  }

  case object UrlType extends PropertyType[String, URL] {
    def read(value: String): Either[PropertyReadError[String], URL] =
      attempt(new URL(value), _ => PropertyReadError(value, "url"))

    def write(value: URL): String = value.toString
  }

  case object JavaFilePathType extends PropertyType[String, java.nio.file.Path] {
    def read(value: String): Either[PropertyReadError[String], java.nio.file.Path] =
      attempt(java.nio.file.Paths.get(value), _ => PropertyReadError(value, " java.nio.file.Path"))

    def write(value: java.nio.file.Path): String = value.toString
  }

  private def attempt[A, E](a: => A, f: Throwable => E): Either[E, A] =
    Try(a) match {
      case Success(value)     => Right(value)
      case Failure(throwable) => Left(f(throwable))
    }
}
