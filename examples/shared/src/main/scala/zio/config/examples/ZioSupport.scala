package zio.config.examples

import zio.Runtime.default
import zio.ZIO

trait ZioSupport {
  implicit class ZioOps[R, E, A](self: ZIO[R, E, A]) {
    def ==(a: A) =
      default.unsafeRun(self) == a
  }
}
