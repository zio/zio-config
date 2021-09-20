package zio.config.cats

import cats._
import zio.config._, ConfigDescriptor._

package object instances {
  implicit val cfgDescInvariantSemiGroupal: InvariantSemigroupal[ConfigDescriptor] =
    new InvariantSemigroupal[ConfigDescriptor] {
      def imap[A, B](fa: ConfigDescriptor[A])(f: A => B)(g: B => A): ConfigDescriptor[B] =
        fa.transform(f, g)

      def product[A, B](fa: ConfigDescriptor[A], fb: ConfigDescriptor[B]): ConfigDescriptor[(A, B)] =
        fa.zip(fb)
    }

  implicit val cfgDescSemiGroupK: SemigroupK[ConfigDescriptor] =
    new SemigroupK[ConfigDescriptor] {
      def combineK[A](x: ConfigDescriptor[A], y: ConfigDescriptor[A]): ConfigDescriptor[A] =
        x.orElse(y)
    }

  implicit val configSourceMonoid: Monoid[ConfigSource] =
    Monoid.instance(ConfigSource.empty, (a, b) => a.orElse(b))

  implicit val configReaderFailureEq: Eq[ReadError[String]] =
    Eq.fromUniversalEquals
}
