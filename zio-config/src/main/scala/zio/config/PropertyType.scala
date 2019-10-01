package zio.config

import java.net.URI

import zio.config.ReadError.{ ErrorType, ParseError }
import zio.config.syntax.AttemptSyntax

trait PropertyType[A] {

  def read(value: String): Either[ErrorType, A]

  def write(a: A): String

  def description: String
}

object PropertyType extends AttemptSyntax {
  def apply[A](implicit ev: PropertyType[A]): PropertyType[A] = ev

  case object StringType extends PropertyType[String] {
    override def description: String                            = "value of type string"
    override def read(value: String): Either[ErrorType, String] = Right(value)
    override def write(a: String): String                       = a
  }

  case object IntType extends PropertyType[Int] {
    def description: String = "value of type int"
    def read(value: String): Either[ErrorType, Int] =
      value.toInt.attempt(_ => ParseError(value, "int"))
    def write(value: Int): String = value.toString
  }

  case object UriType extends PropertyType[URI] {
    def description: String = "value of type uri"
    def read(value: String): Either[ErrorType, URI] =
      new URI(value).attempt(_ => ParseError(value, "uri"))
    def write(value: URI): String = value.toString
  }

  case object LongType extends PropertyType[Long] {
    def description: String = "value of type long"
    def read(value: String): Either[ErrorType, Long] =
      value.toLong.attempt(_ => ParseError(value, "long"))
    def write(value: Long): String = value.toString
  }

  case object ShortType extends PropertyType[Short] {
    def description: String = "value of type short"
    def read(value: String): Either[ErrorType, Short] =
      value.toShort.attempt(_ => ParseError(value, "short"))
    def write(value: Short): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def description: String = "value of type double"
    def read(value: String): Either[ErrorType, Double] =
      value.toDouble.attempt(_ => ParseError(value, "double"))
    def write(value: Double): String = value.toString
  }

  private[config] def ofOption[A](propertyType: PropertyType[A]): PropertyType[Option[A]] =
    new PropertyType[Option[A]] {
      override def read(value: String): Either[ReadError.ErrorType, Option[A]] =
        propertyType.read(value) match {
          case Right(v) => Right(Some(v))
          case Left(v)  => Left(v)
        }
      override def write(a: Option[A]): String = a.map(aa => propertyType.write(aa)).getOrElse("")
      override def description: String         = s"option of ${propertyType.description}"
    }
}
