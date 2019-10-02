package zio.config

import java.net.URI

import zio.config.ReadError.{ ErrorType, ParseError }
import zio.config.syntax.AttemptSyntax

trait PropertyType[A] {
  def read(propertyValue: String): Either[ErrorType, A]
  def write(a: A): String
}

object PropertyType extends AttemptSyntax {

  case object StringType extends PropertyType[String] {
    override def read(value: String): Either[ErrorType, String] = Right(value)
    override def write(a: String): String                       = a
  }

  case object BooleanType extends PropertyType[Boolean] {
    def read(value: String): Either[ErrorType, Boolean] =
      value.toBoolean.attempt(_ => ParseError(value, "boolean"))
    def write(value: Boolean): String = value.toString
  }

  case object ByteType extends PropertyType[Byte] {
    def read(value: String): Either[ErrorType, Byte] =
      value.toByte.attempt(_ => ParseError(value, "byte"))
    def write(value: Byte): String = value.toString
  }

  case object ShortType extends PropertyType[Short] {
    def read(value: String): Either[ErrorType, Short] =
      value.toShort.attempt(_ => ParseError(value, "short"))
    def write(value: Short): String = value.toString
  }

  case object IntType extends PropertyType[Int] {
    def read(value: String): Either[ErrorType, Int] =
      value.toInt.attempt(_ => ParseError(value, "int"))
    def write(value: Int): String = value.toString
  }

  case object LongType extends PropertyType[Long] {
    def read(value: String): Either[ErrorType, Long] =
      value.toLong.attempt(_ => ParseError(value, "long"))
    def write(value: Long): String = value.toString
  }

  case object BigIntType extends PropertyType[BigInt] {
    def read(value: String): Either[ErrorType, BigInt] =
      BigInt(value).attempt(_ => ParseError(value, "bigint"))
    def write(value: BigInt): String = value.toString
  }

  case object FloatType extends PropertyType[Float] {
    def read(value: String): Either[ErrorType, Float] =
      value.toFloat.attempt(_ => ParseError(value, "float"))
    def write(value: Float): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def read(value: String): Either[ErrorType, Double] =
      value.toDouble.attempt(_ => ParseError(value, "double"))
    def write(value: Double): String = value.toString
  }

  case object BigDecimalType extends PropertyType[BigDecimal] {
    def read(value: String): Either[ErrorType, BigDecimal] =
      BigDecimal(value).attempt(_ => ParseError(value, "bigdecimal"))
    def write(value: BigDecimal): String = value.toString
  }

  case object UriType extends PropertyType[URI] {
    def read(value: String): Either[ErrorType, URI] =
      new URI(value).attempt(_ => ParseError(value, "uri"))
    def write(value: URI): String = value.toString
  }
}
