package zio.config.cats

import _root_.cats.kernel.Semigroup
import cats._
import zio.{Config, ConfigProvider}

package object instances {
  implicit val cfgDescInvariantSemiGroupal: Functor[Config] =
    new Applicative[Config] {
      override def ap[A, B](ff: Config[A => B])(fa: Config[A]): Config[B] =
        ff.zip(fa).map { case (f, a) => f(a) }

      override def pure[A](x: A): Config[A] =
        Config.succeed(x)
    }

  implicit val cfgDescSemiGroupK: SemigroupK[Config] =
    new SemigroupK[Config] {
      def combineK[A](x: Config[A], y: Config[A]): Config[A] =
        x.orElse(y)
    }

  implicit val configSourceMonoid: Semigroup[ConfigProvider] =
    new Semigroup[ConfigProvider] {
      override def combine(x: ConfigProvider, y: ConfigProvider): ConfigProvider =
        x.orElse(y)
    }

  implicit val configReaderFailureEq: Eq[Config.Error] =
    Eq.fromUniversalEquals
}
