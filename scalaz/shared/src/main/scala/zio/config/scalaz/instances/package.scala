package zio.config.scalaz

import _root_.scalaz.Applicative
import zio.Config

package object instances {
  implicit val invariantConfig: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A): Config[A] =
        Config.succeed(a)

      override def ap[A, B](fa: => Config[A])(f: => Config[A => B]): Config[B] =
        fa.zip(f).map({ case (a, f) => f(a) })
    }
}
