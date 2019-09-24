package zio.config.syntax

import scala.util.Try

trait AttemptSyntax {
  implicit class ConfigSyntaxSyntaxOps[A](self: => A) {

    def attempt[E](f: Throwable => E): Either[E, A] =
      Try { self } match {
        case scala.util.Success(value)     => Right(value)
        case scala.util.Failure(throwable) => Left(f(throwable))
      }
  }
}
