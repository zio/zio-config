package zio.config

import java.net.URI

import zio.config.ReadErrors.ReadError
import zio.config.ReadErrors.ReadError.ParseError

import scala.util.{ Failure, Success, Try }

trait PropertyType[A] {
  def read(path: String, propertyValue: String): Either[ReadError[String, String], A]
  def write(a: A): String
}

object PropertyType {

  case object StringType extends PropertyType[String] {
    override def read(path: String, value: String): Either[ReadError[String, String], String] = Right(value)
    override def write(a: String): String                                                     = a
  }

  case object BooleanType extends PropertyType[Boolean] {
    def read(path: String, value: String): Either[ReadError[String, String], Boolean] =
      attempt(value.toBoolean, _ => ParseError(path, value, "boolean"))
    def write(value: Boolean): String = value.toString
  }

  case object ByteType extends PropertyType[Byte] {
    def read(path: String, value: String): Either[ReadError[String, String], Byte] =
      attempt(value.toByte, _ => ParseError(path, value, "byte"))
    def write(value: Byte): String = value.toString
  }

  case object ShortType extends PropertyType[Short] {
    def read(path: String, value: String): Either[ReadError[String, String], Short] =
      attempt(value.toShort, _ => ParseError(path, value, "short"))
    def write(value: Short): String = value.toString
  }

  case object IntType extends PropertyType[Int] {
    def read(path: String, value: String): Either[ReadError[String, String], Int] =
      attempt(value.toInt, _ => ParseError(path, value, "int"))
    def write(value: Int): String = value.toString
  }

  case object LongType extends PropertyType[Long] {
    def read(path: String, value: String): Either[ReadError[String, String], Long] =
      attempt(value.toLong, _ => ParseError(path, value, "long"))
    def write(value: Long): String = value.toString
  }

  case object BigIntType extends PropertyType[BigInt] {
    def read(path: String, value: String): Either[ReadError[String, String], BigInt] =
      attempt(BigInt(value), _ => ParseError(path, value, "bigint"))
    def write(value: BigInt): String = value.toString
  }

  case object FloatType extends PropertyType[Float] {
    def read(path: String, value: String): Either[ReadError[String, String], Float] =
      attempt(value.toFloat, _ => ParseError(path, value, "float"))
    def write(value: Float): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def read(path: String, value: String): Either[ReadError[String, String], Double] =
      attempt(value.toDouble, _ => ParseError(path, value, "double"))
    def write(value: Double): String = value.toString
  }

  case object BigDecimalType extends PropertyType[BigDecimal] {
    def read(path: String, value: String): Either[ReadError[String, String], BigDecimal] =
      attempt(BigDecimal(value), _ => ParseError(path, value, "bigdecimal"))
    def write(value: BigDecimal): String = value.toString
  }

  case object UriType extends PropertyType[URI] {
    def read(path: String, value: String): Either[ReadError[String, String], URI] =
      attempt(new URI(value), _ => ParseError(path, value, "uri"))
    def write(value: URI): String = value.toString
  }

  def attempt[A, E](a: => A, f: Throwable => E): Either[E, A] =
    Try(a) match {
      case Success(value)     => Right(value)
      case Failure(throwable) => Left(f(throwable))
    }
}
