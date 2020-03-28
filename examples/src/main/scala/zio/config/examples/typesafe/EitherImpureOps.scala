package zio.config.examples.typesafe

trait EitherImpureOps {
  implicit class EitherImpureOps[A, B](self: Either[A, B]) {
    def loadOrThrow: B =
      self.fold(a => throw new Exception(a.toString), identity)
  }
}
