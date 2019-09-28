package zio.config.syntax

import scala.util.Try

trait AttemptSyntax {
  implicit class ConfigSyntaxSyntaxOps[A](self: => A) {

    def attempt[E](f: Throwable => E): Either[E, A] =
      Try(self).fold(e => Left(f(e)), Right(_))
  }
}
