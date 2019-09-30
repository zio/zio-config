package zio.config

import java.net.URI

import zio.config.ReadError.{ ErrorType, ParseError }
import zio.config.syntax.AttemptSyntax

trait PropertyType[A] {

  def read(propertyValue: String): Either[ErrorType, A]

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

  private[config] def ofList[A](propertyType: PropertyType[A]): PropertyType[List[A]] = new PropertyType[List[A]] {
    override def read(value: String): Either[ErrorType, List[A]] =
      StringType.read(value) match {
        case Right(v) => sequence(v.split(",").toList.map(t => propertyType.read(t)))
        case Left(f)  => Left(f)
      }
    override def write(a: List[A]): String = a.map(aa => propertyType.write(aa)).mkString(",")
    override def description: String       = s"list of ${propertyType.description}"
  }

  private def map2[E, A, B, C](fa: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
    fa match {
      case Right(v1) =>
        b match {
          case Right(v2)  => Right(f(v1, v2))
          case Left(fail) => Left(fail)
        }
      case Left(fail) => Left(fail)
    }

  private[config] def sequence[A, B](fa: List[Either[A, B]]): Either[A, List[B]] =
    fa.foldRight(Right[A, List[B]](Nil: List[B]): Either[A, List[B]])((a, b) => map2(a, b)((c, d) => c :: d))
}
