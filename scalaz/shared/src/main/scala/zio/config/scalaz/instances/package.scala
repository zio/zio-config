package zio.config.scalaz

import scalaz.InvariantFunctor
import zio.config._, ConfigDescriptor._

package object instances {
  implicit val invariantConfigDescriptor: InvariantFunctor[ConfigDescriptor] =
    new InvariantFunctor[ConfigDescriptor] {
      def xmap[A, B](ma: ConfigDescriptor[A], f: A => B, g: B => A): ConfigDescriptor[B] =
        ma.transform(f, g)
    }
}
