package zio.config

import java.net.URI

import zio.config.PropertyType.PropertyReadError

import scala.util.{ Failure, Success, Try }

trait PropertyType[V, A] {
  def read(propertyValue: V): Either[PropertyReadError[V], A]
  def write(a: A): V
}

object PropertyType {
  final case class PropertyReadError[V](value: V, typeInfo: String)

  case object StringType extends PropertyType[String, String] {
    override def read(value: String): Either[PropertyReadError[String], String] = Right(value)
    override def write(a: String): String                                       = a
  }

  case object BooleanType extends PropertyType[String, Boolean] {
    def read(value: String): Either[PropertyReadError[String], Boolean] =
      attempt(value.toBoolean, _ => PropertyReadError(value, "boolean"))
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

  def attempt[A, E](a: => A, f: Throwable => E): Either[E, A] =
    Try(a) match {
      case Success(value)     => Right(value)
      case Failure(throwable) => Left(f(throwable))
    }
}
