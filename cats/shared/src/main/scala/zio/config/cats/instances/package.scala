package zio.config.cats

import cats._
import zio.config._

package object instances {
  implicit val cfgDescInvariantSemiGroupal: InvariantSemigroupal[Config] =
    new InvariantSemigroupal[Config] {
      def imap[A, B](fa: Config[A])(f: A => B)(g: B => A): Config[B] =
        fa.transform(f, g)

      def product[A, B](fa: Config[A], fb: Config[B]): Config[(A, B)] =
        fa.zip(fb)
    }

  implicit val cfgDescSemiGroupK: SemigroupK[Config] =
    new SemigroupK[Config] {
      def combineK[A](x: Config[A], y: Config[A]): Config[A] =
        x.orElse(y)
    }

  implicit val configSourceMonoid: Monoid[ConfigSource] =
    Monoid.instance(ConfigSource.empty, (a, b) => a.orElse(b))

  implicit val configReaderFailureEq: Eq[Config.Error] =
    Eq.fromUniversalEquals
}
