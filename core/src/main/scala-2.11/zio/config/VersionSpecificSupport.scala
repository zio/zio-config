package zio.config

private[config] object VersionSpecificSupport {
  implicit class RightBiasedEither[L, R](e: Either[L, R]) {
    def map[R2](f: R => R2): Either[L, R2] = e match {
      case Left(a)  => Left(a)
      case Right(b) => Right(f(b))
    }
  }
}
