package zio.config

import java.net.URI

import zio.config.ReadError.ParseError
import zio.config.syntax.AttemptSyntax

trait PropertyType[A] {

  def read(path: String, propertyValue: String): Either[ReadError, A]

  def write(a: A): String

  def description: String
}

object PropertyType extends AttemptSyntax {
  def apply[A](implicit ev: PropertyType[A]): PropertyType[A] = ev

  case object StringType extends PropertyType[String] {
    override def description: String                                          = "value of type string"
    override def read(path: String, value: String): Either[ReadError, String] = Right(value)
    override def write(a: String): String                                     = a
  }

  case object BooleanType extends PropertyType[Boolean] {
    def description: String = "value of type boolean"
    def read(path: String, value: String): Either[ReadError, Boolean] =
      value.toBoolean.attempt(_ => ParseError(path, value, "boolean"))
    def write(value: Boolean): String = value.toString
  }

  case object ByteType extends PropertyType[Byte] {
    def description: String = "value of type byte"
    def read(path: String, value: String): Either[ReadError, Byte] =
      value.toByte.attempt(_ => ParseError(path, value, "byte"))
    def write(value: Byte): String = value.toString
  }

  case object ShortType extends PropertyType[Short] {
    def description: String = "value of type short"
    def read(path: String, value: String): Either[ReadError, Short] =
      value.toShort.attempt(_ => ParseError(path, value, "short"))
    def write(value: Short): String = value.toString
  }

  case object IntType extends PropertyType[Int] {
    def description: String = "value of type int"
    def read(path: String, value: String): Either[ReadError, Int] =
      value.toInt.attempt(_ => ParseError(path, value, "int"))
    def write(value: Int): String = value.toString
  }

  case object LongType extends PropertyType[Long] {
    def description: String = "value of type long"
    def read(path: String, value: String): Either[ReadError, Long] =
      value.toLong.attempt(_ => ParseError(path, value, "long"))
    def write(value: Long): String = value.toString
  }

  case object BigIntType extends PropertyType[BigInt] {
    def description: String = "value of type bigint"
    def read(path: String, value: String): Either[ReadError, BigInt] =
      BigInt(value).attempt(_ => ParseError(path, value, "bigint"))
    def write(value: BigInt): String = value.toString
  }

  case object FloatType extends PropertyType[Float] {
    def description: String = "value of type float"
    def read(path: String, value: String): Either[ReadError, Float] =
      value.toFloat.attempt(_ => ParseError(path, value, "float"))
    def write(value: Float): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def description: String = "value of type double"
    def read(path: String, value: String): Either[ReadError, Double] =
      value.toDouble.attempt(_ => ParseError(path, value, "double"))
    def write(value: Double): String = value.toString
  }

  case object BigDecimalType extends PropertyType[BigDecimal] {
    def description: String = "value of type bigdecimal"
    def read(path: String, value: String): Either[ReadError, BigDecimal] =
      BigDecimal(value).attempt(_ => ParseError(path, value, "bigdecimal"))
    def write(value: BigDecimal): String = value.toString
  }

  case object UriType extends PropertyType[URI] {
    def description: String = "value of type uri"
    def read(path: String, value: String): Either[ReadError, URI] =
      new URI(value).attempt(_ => ParseError(path, value, "uri"))
    def write(value: URI): String = value.toString
  }
}
