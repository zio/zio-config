package zio.config.typesafe

trait EitherSupport {
  implicit class ImpureEitherOps[A, B](s: Either[A, B]) {
    def loadOrThrow: B = s.fold(r => throw new Exception(r.toString), identity)
  }
}
