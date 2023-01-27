package zio.config.scalaz

import scalaz.InvariantFunctor
import zio.config._
import zio.Config
import _root_.scalaz.Applicative

package object instances {
  implicit val invariantConfig: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A): Config[A] =
        Config.succeed(a)

      override def ap[A, B](fa: => Config[A])(f: => Config[A => B]): Config[B] =
        fa.zip(f).map({ case (a, f) => f(a) })
    }
}
