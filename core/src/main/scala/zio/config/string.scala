package zio.config

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import scala.concurrent.duration.Duration

trait StringConfigModule extends ReadFunctions with ConfigDocsFunctions {
  type K = String
  type V = String

  val bigDecimal: ConfigDescriptor[BigDecimal] =
    Source(ConfigSource.empty, PropertyType.BigDecimalType) ?? "value of type bigdecimal"

  def bigDecimal(path: String): ConfigDescriptor[BigDecimal] = nested(path)(bigDecimal)

  val bigInt: ConfigDescriptor[BigInt] =
    Source(ConfigSource.empty, PropertyType.BigIntType) ?? "value of type bigint"

  def bigInt(path: String): ConfigDescriptor[BigInt] = nested(path)(bigInt)

  val boolean: ConfigDescriptor[Boolean] =
    Source(ConfigSource.empty, PropertyType.BooleanType) ?? "value of type boolean"

  def boolean(path: String): ConfigDescriptor[Boolean] = nested(path)(boolean)

  val byte: ConfigDescriptor[Byte] =
    Source(ConfigSource.empty, PropertyType.ByteType) ?? "value of type byte"

  def byte(path: String): ConfigDescriptor[Byte] = nested(path)(byte)

  def collectAll[A](
    head: ConfigDescriptor[A],
    tail: ConfigDescriptor[A]*
  ): ConfigDescriptor[List[A]] =
    sequence(head, tail: _*)({ case (a, t) => a :: t }, l => l.headOption.map(h => (h, l.tail)))

  val double: ConfigDescriptor[Double] =
    Source(ConfigSource.empty, PropertyType.DoubleType) ?? "value of type double"

  def double(path: String): ConfigDescriptor[Double] = nested(path)(double)

  val duration: ConfigDescriptor[Duration] =
    Source(ConfigSource.empty, PropertyType.DurationType) ?? "value of type duration"

  def duration(path: String): ConfigDescriptor[Duration] = nested(path)(duration)

  val zioDuration: ConfigDescriptor[zio.duration.Duration] =
    Source(ConfigSource.empty, PropertyType.ZioDurationType) ?? "value of type duration"

  def zioDuration(path: String): ConfigDescriptor[zio.duration.Duration] = nested(path)(zioDuration)

  val float: ConfigDescriptor[Float] =
    Source(ConfigSource.empty, PropertyType.FloatType) ?? "value of type float"

  def float(path: String): ConfigDescriptor[Float] = nested(path)(float)

  val int: ConfigDescriptor[Int] =
    Source(ConfigSource.empty, PropertyType.IntType) ?? "value of type int"

  def int(path: String): ConfigDescriptor[Int] = nested(path)(int)

  val short: ConfigDescriptor[Short] =
    Source(ConfigSource.empty, PropertyType.ShortType) ?? "value of type short"

  def short(path: String): ConfigDescriptor[Short] = nested(path)(short)

  val string: ConfigDescriptor[String] =
    Source(ConfigSource.empty, PropertyType.StringType) ?? "value of type string"

  def string(path: String): ConfigDescriptor[String] = nested(path)(string)

  val uri: ConfigDescriptor[URI] =
    Source(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

  def uri(path: String): ConfigDescriptor[URI] = nested(path)(uri)

  val uuid: ConfigDescriptor[UUID] =
    Source(ConfigSource.empty, PropertyType.UuidType) ?? "value of type uuid"

  def uuid(path: String): ConfigDescriptor[UUID] = nested(path)(uuid)

  val localDate: ConfigDescriptor[LocalDate] =
    Source(ConfigSource.empty, PropertyType.LocalDateType) ?? "value of type localdate"

  def localDate(path: String): ConfigDescriptor[LocalDate] = nested(path)(localDate)

  val localTime: ConfigDescriptor[LocalTime] =
    Source(ConfigSource.empty, PropertyType.LocalTimeType) ?? "value of type localtime"

  def localTime(path: String): ConfigDescriptor[LocalTime] = nested(path)(localTime)

  val localDateTime: ConfigDescriptor[LocalDateTime] =
    Source(ConfigSource.empty, PropertyType.LocalDateTimeType) ?? "value of type localdatetime"

  def localDateTime(path: String): ConfigDescriptor[LocalDateTime] = nested(path)(localDateTime)

  val long: ConfigDescriptor[Long] =
    Source(ConfigSource.empty, PropertyType.LongType) ?? "value of type long"

  def long(path: String): ConfigDescriptor[Long] = nested(path)(long)

  val instant: ConfigDescriptor[Instant] =
    Source(ConfigSource.empty, PropertyType.InstantType) ?? "value of type instant"

  def instant(path: String): ConfigDescriptor[Instant] = nested(path)(instant)

  val file: ConfigDescriptor[File] =
    Source(ConfigSource.empty, PropertyType.FileType) ?? "value of type file"

  def file(path: String): ConfigDescriptor[File] = nested(path)(file)

  val url: ConfigDescriptor[URL] =
    Source(ConfigSource.empty, PropertyType.UrlType) ?? "value of type URL"

  def url(path: String): ConfigDescriptor[URL] = nested(path)(url)

}

object string extends StringConfigModule