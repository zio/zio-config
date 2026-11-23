package zio.config.scalaz

import _root_.scalaz.Applicative
import zio.Config

/**
 * Type class instances from scalaz for `Config`. Import them with `import zio.config.scalaz.instances._`.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object instances {

  /**
   * An `Applicative[Config]` whose `point` is `Config.succeed` and whose `ap` reads both descriptions with
   * `Config#zip`, so scalaz functions such as `apply2` combine descriptions. The combined description fails when either
   * one fails.
   *
   * {{{
   * import zio._
   * import zio.config.scalaz.instances._
   * import _root_.scalaz.Applicative
   *
   * val total: Config[Int] = Applicative[Config].apply2(Config.int("A"), Config.int("B"))(_ + _)
   *
   * val loaded: IO[Config.Error, Int] = ConfigProvider.fromMap(Map("A" -> "1", "B" -> "2")).load(total)
   * // succeeds with 3
   * }}}
   */
  implicit val invariantConfig: Applicative[Config] =
    new Applicative[Config] {
      override def point[A](a: => A): Config[A] =
        Config.succeed(a)

      override def ap[A, B](fa: => Config[A])(f: => Config[A => B]): Config[B] =
        fa.zip(f).map { case (a, f) => f(a) }
    }
}
