package zio.config

import _root_.scalaz.{==>>, IList, ISet, Maybe, Order}
import zio.Config

import Config._

/**
 * Descriptions that read scalaz data types: `IList`, `ISet`, `==>>` and `Maybe`. An `Applicative[Config]` is in
 * `zio.config.scalaz.instances`.
 *
 * After `import zio.config._`, the name `scalaz` refers to this package and hides the library's own `scalaz` package,
 * so write the scalaz imports with `_root_`, as in `import _root_.scalaz.IList`.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object scalaz {

  /**
   * Reads a list of `config` values, as `Config.listOf` does, into an `IList`. It takes no key; add one with
   * `Config#nested`.
   *
   * {{{
   * import zio._
   * import zio.config.scalaz._
   * import _root_.scalaz.IList
   *
   * val tags: Config[IList[String]] = iList(Config.string).nested("tags")
   *
   * val loaded: IO[Config.Error, List[String]] =
   *   ConfigProvider.fromMap(Map("tags[0]" -> "blue", "tags[1]" -> "green")).load(tags).map(_.toList)
   * // succeeds with List(blue, green)
   * }}}
   */
  def iList[A](config: Config[A]): Config[IList[A]] =
    listOf(config).map(IList.fromList)

  /**
   * Reads a list of `config` values, as `Config.listOf` does, into an `ISet` ordered by the `Order[A]`; duplicates are
   * dropped. It takes no key; add one with `Config#nested`.
   *
   * {{{
   * import zio._
   * import zio.config.scalaz._
   * import _root_.scalaz.ISet
   * import _root_.scalaz.std.anyVal._
   *
   * val ids: Config[ISet[Int]] = iSet(Config.int).nested("ids")
   *
   * val loaded: IO[Config.Error, List[Int]] =
   *   ConfigProvider.fromMap(Map("ids[0]" -> "3", "ids[1]" -> "1", "ids[2]" -> "3")).load(ids).map(_.toList)
   * // succeeds with List(1, 3)
   * }}}
   */
  def iSet[A: Order](config: Config[A]): Config[ISet[A]] =
    listOf(config).map(l => ISet.fromList(l))

  /**
   * Converts a description of a `Map`, such as one from `Config.table`, into a description of a scalaz `==>>` map
   * ordered by the `Order[A]`.
   *
   * {{{
   * import zio._
   * import zio.config.scalaz._
   * import _root_.scalaz.==>>
   * import _root_.scalaz.std.string._
   *
   * val ports: Config[String ==>> Int] = mapz(Config.table("ports", Config.int))
   *
   * val loaded: IO[Config.Error, List[(String, Int)]] =
   *   ConfigProvider.fromMap(Map("ports.http" -> "80", "ports.admin" -> "9000")).load(ports).map(_.toList)
   * // succeeds with List((admin,9000), (http,80))
   * }}}
   */
  def mapz[A: Order, B](config: Config[Map[A, B]]): Config[A ==>> B] =
    config.map(m => ==>>.fromList(m.toList))

  /**
   * Makes `config` optional, as `Config#optional` does, with a scalaz `Maybe`: a missing value gives `Maybe.Empty`, a
   * value gives `Maybe.Just`, and a value that `config` can't read still fails.
   *
   * {{{
   * import zio._
   * import zio.config.scalaz._
   * import _root_.scalaz.Maybe
   *
   * val age: Config[Maybe[Int]] = maybe(Config.int("AGE"))
   *
   * val missing: IO[Config.Error, Maybe[Int]] = ConfigProvider.fromMap(Map.empty).load(age)
   * // succeeds with Empty()
   * val present: IO[Config.Error, Maybe[Int]] = ConfigProvider.fromMap(Map("AGE" -> "42")).load(age)
   * // succeeds with Just(42)
   * }}}
   */
  def maybe[A](config: Config[A]): Config[Maybe[A]] =
    config.optional.map(Maybe.fromOption)
}
