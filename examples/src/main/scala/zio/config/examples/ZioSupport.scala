package zio.config.examples

import zio.Runtime.default
import zio.{Unsafe, ZIO}

trait ZioSupport {
  implicit class ZioOps[E, A](self: ZIO[Any, E, A]) {
    def unsafeRun: A =
      Unsafe.unsafe(implicit u => default.unsafe.run(self).getOrThrowFiberFailure())

    def equalM(a: A): Boolean = {
      val result = unsafeRun == a
      if (!result) {
        println("Expected")
        println(a)
        println("Actual")
        println(unsafeRun)
      }

      result
    }
  }
}
