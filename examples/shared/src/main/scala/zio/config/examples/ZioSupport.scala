package zio.config.examples

import zio.Runtime.default
import zio.ZIO

trait ZioSupport {
  implicit class ZioOps[E, A](self: ZIO[Any, E, A]) {
    def equalM(a: A) =
      default.unsafeRun(self) == a
  }
}
