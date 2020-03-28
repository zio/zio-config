package zio.config

private[config] trait EitherFunctions {
  implicit class EitherOps[A, B](self: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = self match {
      case Left(value)  => Left(value)
      case Right(value) => Right(f(value))
    }

    def flatMap[C](f: B => Either[A, C]): Either[A, C] =
      self match {
        case Left(value)  => Left(value)
        case Right(value) => f(value)
      }
  }

}
