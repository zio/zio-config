package zio.config.cats

import _root_.cats.kernel.Semigroup
import cats._
import zio.{Config, ConfigProvider}

/**
 * Type class instances from cats for `Config`, `ConfigProvider` and `Config.Error`. Import them with
 * `import zio.config.cats.instances._`.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object instances {

  /**
   * A `Functor[Config]`. The value behind it is an `Applicative` whose `pure` is `Config.succeed` and whose `ap` reads
   * both descriptions with `Config#zip`, but it is declared as a `Functor`, so implicit search finds only a `Functor`:
   * syntax such as `mapN` or `tupled` doesn't compile for `Config`.
   *
   * {{{
   * import zio._
   * import zio.config.cats.instances._
   * import _root_.cats.Functor
   *
   * val portAndHost: Config[(Int, String)] = Functor[Config].fproduct(Config.int("PORT"))(port => "localhost:" + port)
   *
   * val loaded: IO[Config.Error, (Int, String)] = ConfigProvider.fromMap(Map("PORT" -> "8080")).load(portAndHost)
   * // succeeds with (8080,localhost:8080)
   * }}}
   */
  implicit val cfgDescInvariantSemiGroupal: Functor[Config] =
    new Applicative[Config] {
      override def ap[A, B](ff: Config[A => B])(fa: Config[A]): Config[B] =
        ff.zip(fa).map { case (f, a) => f(a) }

      override def pure[A](x: A): Config[A] =
        Config.succeed(x)
    }

  /**
   * A `SemigroupK[Config]` whose `combineK`, written `<+>`, is `Config#orElse`: it reads the first description and,
   * when that fails, the second.
   *
   * {{{
   * import zio._
   * import zio.config.cats.instances._
   * import _root_.cats.syntax.all._
   *
   * val url: Config[String] = Config.string("DATABASE_URL") <+> Config.string("DB_URL")
   *
   * val loaded: IO[Config.Error, String] = ConfigProvider.fromMap(Map("DB_URL" -> "postgres://localhost/app")).load(url)
   * // succeeds with postgres://localhost/app
   * }}}
   */
  implicit val cfgDescSemiGroupK: SemigroupK[Config] =
    new SemigroupK[Config] {
      def combineK[A](x: Config[A], y: Config[A]): Config[A] =
        x.orElse(y)
    }

  /**
   * A `Semigroup[ConfigProvider]` (not a `Monoid`) whose `combine`, written `|+|`, is `ConfigProvider#orElse`: a key
   * missing from the first provider is read from the second.
   *
   * {{{
   * import zio._
   * import zio.config.cats.instances._
   * import _root_.cats.syntax.all._
   *
   * val provider: ConfigProvider =
   *   ConfigProvider.fromMap(Map("HOST" -> "db.internal")) |+| ConfigProvider.fromMap(Map("HOST" -> "localhost", "PORT" -> "5432"))
   *
   * val loaded: IO[Config.Error, (String, Int)] = provider.load(Config.string("HOST").zip(Config.int("PORT")))
   * // succeeds with (db.internal,5432)
   * }}}
   */
  implicit val configSourceMonoid: Semigroup[ConfigProvider] =
    new Semigroup[ConfigProvider] {
      override def combine(x: ConfigProvider, y: ConfigProvider): ConfigProvider =
        x.orElse(y)
    }

  /**
   * An `Eq[Config.Error]` that compares errors with `==`, so `===` works on errors.
   *
   * {{{
   * import zio._
   * import zio.config.cats.instances._
   * import _root_.cats.syntax.all._
   *
   * val missing: Config.Error = Config.Error.MissingData(Chunk("PORT"), "Expected PORT to be set")
   *
   * val same: Boolean = missing === Config.Error.MissingData(Chunk("PORT"), "Expected PORT to be set")
   * // true
   * }}}
   */
  implicit val configReaderFailureEq: Eq[Config.Error] =
    Eq.fromUniversalEquals
}
