package zio.config.examples

import zio.Runtime.default
import zio.ZIO

trait ZioSupport {
  implicit class ZioOps[E, A](self: ZIO[Any, E, A]) {
    def unsafeRun =
      default.unsafeRun(self)

    def equalM(a: A) =
      unsafeRun == a
  }
}
