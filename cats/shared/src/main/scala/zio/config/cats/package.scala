package zio.config

import _root_.cats.data._
import _root_.cats.implicits._
import _root_.cats.kernel.Order
import zio.Config

import scala.collection.immutable.SortedMap

import Config._

/**
 * Descriptions that read cats data types: `Chain`, `NonEmptyChain`, `NonEmptyList` and `NonEmptyMap`. Each function
 * takes the description of one element, with or without the name of the key that holds the collection. Type class
 * instances for `Config` are in `zio.config.cats.instances`.
 *
 * After `import zio.config._`, the name `cats` refers to this package and hides the library's own `cats` package, so
 * write the cats imports with `_root_`, as in `import _root_.cats.data.NonEmptyList`.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object cats {

  /**
   * Reads a list of `aDesc` values, as `Config.listOf` does, into a `Chain`. An empty list, such as YAML `[]`, gives an
   * empty `Chain`.
   *
   * {{{
   * import zio._
   * import zio.config.cats._
   * import _root_.cats.data.Chain
   *
   * val hosts: Config[Chain[String]] = chain(Config.string).nested("hosts")
   *
   * val loaded: IO[Config.Error, Chain[String]] =
   *   ConfigProvider.fromMap(Map("hosts[0]" -> "a.internal", "hosts[1]" -> "b.internal")).load(hosts)
   * // succeeds with Chain(a.internal, b.internal)
   * }}}
   */
  def chain[A](aDesc: Config[A]): Config[Chain[A]] =
    listOf(aDesc).map(v => Chain.fromSeq(v))

  /**
   * Reads a list of `aDesc` values at the key `path` into a `Chain`; the same as `chain(aDesc).nested(path)`.
   */
  def chain[A](path: String)(aDesc: Config[A]): Config[Chain[A]] =
    chain(aDesc).nested(path)

  /**
   * Reads a list of `aDesc` values, as `Config.listOf` does, into a `NonEmptyChain`. An empty list fails with
   * `Config.Error.InvalidData` and the message `chain is empty`.
   */
  def nonEmptyChain[A](aDesc: Config[A]): Config[NonEmptyChain[A]] =
    chain(aDesc).mapOrFail(value =>
      NonEmptyChain
        .fromChain(value)
        .fold[Either[Config.Error, NonEmptyChain[A]]](Left(Config.Error.InvalidData(message = "chain is empty")))(
          value => Right(value)
        )
    )

  /**
   * Reads a list of `aDesc` values at the key `path` into a `NonEmptyChain`; the same as
   * `nonEmptyChain(aDesc).nested(path)`. An empty list fails with the message `chain is empty`.
   */
  def nonEmptyChain[A](path: String)(aDesc: Config[A]): Config[NonEmptyChain[A]] =
    nonEmptyChain(aDesc).nested(path)

  /**
   * Reads a list of `aDesc` values, as `Config.listOf` does, into a `NonEmptyList`. An empty list, such as YAML `[]`,
   * fails with `Config.Error.InvalidData` and the message `list is empty`.
   */
  def nonEmptyList[A](aDesc: Config[A]): Config[NonEmptyList[A]] =
    listOf(aDesc).mapOrFail(value =>
      NonEmptyList
        .fromList(value)
        .fold[Either[Config.Error, NonEmptyList[A]]](Left(Config.Error.InvalidData(message = "list is empty")))(v =>
          Right(v)
        )
    )

  /**
   * Reads a list of `aDesc` values at the key `path` into a `NonEmptyList`; the same as
   * `nonEmptyList(aDesc).nested(path)`. An empty list fails with the message `list is empty`.
   *
   * {{{
   * import zio._
   * import zio.config.cats._
   * import _root_.cats.data.NonEmptyList
   *
   * val ports: Config[NonEmptyList[Int]] = nonEmptyList("ports")(Config.int)
   *
   * val loaded: IO[Config.Error, NonEmptyList[Int]] =
   *   ConfigProvider.fromMap(Map("ports[0]" -> "8080", "ports[1]" -> "8443")).load(ports)
   * // succeeds with NonEmptyList(8080, 8443)
   * }}}
   */
  def nonEmptyList[A](path: String)(aDesc: Config[A]): Config[NonEmptyList[A]] =
    nonEmptyList(aDesc).nested(path)

  /**
   * Reads a table of `aDesc` values, as `Config.table` does, into a `NonEmptyMap` whose keys are sorted. An empty table
   * fails with `Config.Error.InvalidData` and the message `map is empty`; with `ConfigProvider.fromMap`, so does a
   * missing key.
   */
  def nonEmptyMap[A](aDesc: Config[A]): Config[NonEmptyMap[String, A]] = {
    // Kept in implicit scope rather than applied positionally: Scala 3 fills the
    // implicit itself and then reads the argument list as a key lookup on the map.
    implicit val ordering: Ordering[String] = Order[String].toOrdering

    table(aDesc).mapOrFail(x =>
      NonEmptyMap
        .fromMap(SortedMap.empty[String, A] ++ x)
        .fold[Either[Config.Error, NonEmptyMap[String, A]]](Left(Config.Error.InvalidData(message = "map is empty")))(
          v => Right(v)
        )
    )
  }

  /**
   * Reads a table of `aDesc` values at the key `path` into a `NonEmptyMap` whose keys are sorted; the same as
   * `nonEmptyMap(aDesc).nested(path)`. An empty table fails with the message `map is empty`.
   *
   * {{{
   * import zio._
   * import zio.config.cats._
   * import _root_.cats.data.NonEmptyMap
   *
   * val timeouts: Config[NonEmptyMap[String, Int]] = nonEmptyMap("timeouts")(Config.int)
   *
   * val loaded: IO[Config.Error, NonEmptyMap[String, Int]] =
   *   ConfigProvider.fromMap(Map("timeouts.read" -> "30", "timeouts.connect" -> "5")).load(timeouts)
   * // succeeds with a map of connect -> 5 and read -> 30
   * }}}
   */
  def nonEmptyMap[A](path: String)(aDesc: Config[A]): Config[NonEmptyMap[String, A]] =
    nonEmptyMap(aDesc).nested(path)
}
