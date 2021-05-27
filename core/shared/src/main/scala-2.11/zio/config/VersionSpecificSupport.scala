package zio.config

import scala.util.{Failure, Success}

private[config] object VersionSpecificSupport {
  implicit class RightBiasedEither[L, R](e: Either[L, R]) {
    def map[R2](f: R => R2): Either[L, R2] = e match {
      case Left(a)  => Left(a)
      case Right(b) => Right(f(b))
    }
  }

  implicit class TryOps[A](t: scala.util.Try[A]) {
    def toEither = t match {
      case Failure(exception) => Left(exception)
      case Success(value)     => Right(value)
    }
  }
}
