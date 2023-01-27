package zio.config.scalaz

import scalaz.InvariantFunctor
import zio.config._

package object instances {
  implicit val invariantConfig: InvariantFunctor[Config] =
    new InvariantFunctor[Config] {
      def xmap[A, B](ma: Config[A], f: A => B, g: B => A): Config[B] =
        ma.transform(f, g)
    }
}
