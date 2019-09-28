package zio.config.syntax

import scala.util.{ Failure, Success, Try }

trait AttemptSyntax {
  implicit class ConfigSyntaxSyntaxOps[A](self: => A) {

    def attempt[E](f: Throwable => E): Either[E, A] =
      Try(self) match {
        case Success(value)     => Right(value)
        case Failure(throwable) => Left(f(throwable))
      }
  }
}
