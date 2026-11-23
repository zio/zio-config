package zio.config

import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.Config
import zio.config.magnolia.DeriveConfig

/**
 * Reads refined types (from the refined library) with ZIO Config. A value that fails the predicate fails with
 * `Config.Error.InvalidData`, whose message is refined's message, such as `Predicate failed: (0 > 0).`. When the
 * refinement is applied to a description that is already nested under a key, as `refine[A, P](path)` and
 * `refineType[R](path)` do, the error's path leaves out that key, and is empty unless the description is nested
 * further.
 *
 * @see
 *   [[https://zio.dev/zio-config/integrations/ Integrations]]
 */
package object refined {

  /**
   * Derives a `DeriveConfig` for `Refined[A, P]` from the `DeriveConfig[A]` of the underlying type and a
   * `Validate[A, P]`, so `deriveConfig` reads case class fields of refined types after `import zio.config.refined._`. A
   * field whose value fails the predicate fails with `Config.Error.InvalidData` at that field's key.
   *
   * {{{
   * import zio._
   * import zio.config.refined._
   * import zio.config.magnolia.deriveConfig
   * import eu.timepit.refined.api.Refined
   * import eu.timepit.refined.numeric.Positive
   * import eu.timepit.refined.types.string.NonEmptyString
   *
   * final case class Server(host: NonEmptyString, port: Int Refined Positive)
   *
   * val valid: IO[Config.Error, Server] =
   *   ConfigProvider.fromMap(Map("host" -> "localhost", "port" -> "8080")).load(deriveConfig[Server])
   * // succeeds with Server(localhost,8080)
   *
   * val invalid: IO[Config.Error, Server] =
   *   ConfigProvider.fromMap(Map("host" -> "localhost", "port" -> "-1")).load(deriveConfig[Server])
   * // fails with (Invalid data at port: Predicate failed: (-1 > 0).)
   * }}}
   */
  implicit def deriveRefinedDescriptor[A, P](implicit
    desc: DeriveConfig[A],
    validate: Validate[A, P]
  ): DeriveConfig[Refined[A, P]] =
    DeriveConfig(refine[P](desc.desc))

  /**
   * Reads the value at the key `path` with the `DeriveConfig[A]` of the underlying type, and refines it with the
   * predicate `P`. When the predicate fails, the error's path leaves out `path`: it is empty unless the description is
   * nested further.
   *
   * {{{
   * import zio._
   * import zio.config.refined._
   * import eu.timepit.refined.api.Refined
   * import eu.timepit.refined.string.Uuid
   *
   * val id: Config[Refined[String, Uuid]] = refine[String, Uuid]("ID")
   *
   * val valid: IO[Config.Error, Refined[String, Uuid]] =
   *   ConfigProvider.fromMap(Map("ID" -> "123e4567-e89b-12d3-a456-426614174000")).load(id)
   * // succeeds with 123e4567-e89b-12d3-a456-426614174000
   *
   * val invalid: IO[Config.Error, Refined[String, Uuid]] = ConfigProvider.fromMap(Map("ID" -> "abc")).load(id)
   * // fails with (Invalid data at : Uuid predicate failed: Invalid UUID string: abc)
   * }}}
   */
  def refine[A, P](path: String)(implicit
    desc: DeriveConfig[A],
    validate: Validate[A, P]
  ): Config[Refined[A, P]] =
    desc.desc
      .nested(path)
      .mapOrFail[Refined[A, P]](v =>
        RefType.applyRef[Refined[A, P]](v).swap.map(str => Config.Error.InvalidData(message = str)).swap
      )

  /**
   * Refines an existing description with the predicate `Predicate`. `refine[P]` returns a value whose `apply` takes a
   * `Config[A]`, where a `Validate[A, P]` exists, and gives a `Config[Refined[A, P]]`. Use it to build the raw
   * description first, for example with `Config.listOf` or `deriveConfig`, and add the predicate afterwards.
   *
   * {{{
   * import zio._
   * import zio.config.refined._
   * import zio.config.magnolia.deriveConfig
   * import eu.timepit.refined.api.Refined
   * import eu.timepit.refined.collection.NonEmpty
   *
   * final case class Database(url: String, port: Int)
   *
   * val databases: Config[List[Database]] = Config.listOf("databases", deriveConfig[Database])
   *
   * val atLeastOne: Config[Refined[List[Database], NonEmpty]] = refine[NonEmpty](databases)
   *
   * val loaded: IO[Config.Error, Refined[List[Database], NonEmpty]] =
   *   ConfigProvider.fromMap(Map("databases[0].url" -> "jdbc:postgresql://db1/app", "databases[0].port" -> "5432")).load(atLeastOne)
   * // succeeds with List(Database(jdbc:postgresql://db1/app,5432))
   * }}}
   *
   * To read a ready-made refined type such as `NonEmptyString` from a key, use `refineType[R]` instead.
   */
  def refine[Predicate]: PartialRefined[Predicate] =
    new PartialRefined[Predicate]

  /**
   * Reads a ready-made refined type `R`, such as `NonEmptyString` (`String Refined NonEmpty`), from a key.
   * `refineType[R]` returns a [[PartialRefinedPath]], whose `apply(path)` reads the value at `path` with the
   * `DeriveConfig` of the underlying type and refines it. `refineType[NonEmptyString]("USERNAME")` gives the same
   * result as `refine[NonEmpty](Config.string("USERNAME"))`.
   *
   * When the predicate fails, the error's path leaves out the key: it is empty unless the description is nested
   * further.
   *
   * {{{
   * import zio._
   * import zio.config._
   * import zio.config.refined._
   * import eu.timepit.refined.types.string.NonEmptyString
   *
   * final case class Jdbc(username: NonEmptyString, password: NonEmptyString)
   *
   * val jdbc: Config[Jdbc] =
   *   refineType[NonEmptyString]("username").zip(refineType[NonEmptyString]("password")).to[Jdbc]
   *
   * val invalid: IO[Config.Error, Jdbc] =
   *   ConfigProvider.fromMap(Map("username" -> "app", "password" -> "")).load(jdbc)
   * // fails with (Invalid data at : Predicate isEmpty() did not fail.)
   * }}}
   */
  def refineType[R]: PartialRefinedPath[R] =
    PartialRefinedPath[R]()
}
