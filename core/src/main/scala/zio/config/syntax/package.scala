package zio.config

import zio.{ Has, Tag, ZLayer }

package object syntax {

  final implicit class ZIOConfigNarrowOps[R, E, A](val self: ZLayer[R, E, Has[A]]) extends AnyVal {
    def narrow[B: Tag](f: A => B)(implicit T: Tag[A]): ZLayer[R, E, Has[B]] =
      self.map(a => Has(f(a.get)))
  }
}
