package zio.config

import zio.config.ConfigError.{ErrorType, ParseError}
import zio.config.syntax.AttemptSyntax

trait PropertyType[A] {

  def read(value: String): Either[ErrorType, A]

  def write(a: A): String

  def description: String
}

object PropertyType extends AttemptSyntax {
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

  case object LongType extends PropertyType[Long] {
    def description: String = "value of type long"
    def read(value: String): Either[ErrorType, Long] =
      value.toLong.attempt(_ => ParseError(value, "long"))
    def write(value: Long): String = value.toString
  }

  case object DoubleType extends PropertyType[Double] {
    def description: String = "value of type double"
    def read(value: String): Either[ErrorType, Double] =
      value.toDouble.attempt(_ => ParseError(value, "double"))
    def write(value: Double): String = value.toString
  }

  private[config] def ofOption[A](propertyType: PropertyType[A]): PropertyType[Option[A]] =
    new PropertyType[Option[A]] {
      override def read(value: String): Either[ConfigError.ErrorType, Option[A]] =
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

  private def sequence[A, B](fa: List[Either[A, B]]): Either[A, List[B]] =
    fa.foldRight(Right[A, List[B]](Nil: List[B]): Either[A, List[B]])((a, b) => map2(a, b)((c, d) => c :: d))
}
