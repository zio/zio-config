package zio.config

import zio.{ Has, Tagged, ZLayer }

package object syntax {

  final implicit class ZIOConfigNarrowOps[R, E, A](val self: ZLayer[R, E, Has[A]]) extends AnyVal {
    def narrow[B: Tagged](f: A => B)(implicit T: Tagged[A]): ZLayer[R, E, Has[B]] =
      self.map(a => Has(f(a.get)))
  }
}
