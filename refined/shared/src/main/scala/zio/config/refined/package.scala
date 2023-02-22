package zio.config

import com.github.ghik.silencer.silent
import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.Config
import zio.config.magnolia.DeriveConfig

package object refined {

  /**
   * FIXME
   * Automatically derive instances of Descriptor for any refined types
   */
  @silent("deprecated")
  implicit def deriveRefinedDescriptor[A, P](implicit
    desc: DeriveConfig[A],
    validate: Validate[A, P]
  ): DeriveConfig[Refined[A, P]] =
    DeriveConfig(refine[P](desc.desc))

  /**
   * `refine` allows us to retrieve a `refined` type from a given path.
   *
   * Example:
   *
   * {{{
   *   import eu.timepit.refined.string._
   *
   *   val configDescriptor: Config[Refined[String, Uuid]] =
   *     refined[String, Uuid]("ID")
   * }}}
   */
  @silent("deprecated")
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
   * refine[Predicate] allows to retrieve a `refined` type given an existing `Config`
   * and a predicate Predicate. Example of a Predicate is `NonEmpty`.
   *
   * Example:
   *
   * {{{
   *
   *   final case class MyConfig(url: String, port: Int)
   *
   *   val configs: Config[List[MyConfig]] =
   *     listOf("databases", deriveConfig[MyConfig])
   *
   *   val configDescriptor: Config[Refined[List[MyConfig], Size[Greater[W.`2`.T]]]] =
   *     refined[Size[Greater[W.`2`.T]]](configs)
   * }}}
   *
   * If you don't care predicates specifically, and need to pass a fully formed Refined type
   * (Example: type NonEmptyString = String Refined NonEmpty), refer `refineType[RefinedType]`
   */
  def refine[Predicate]: PartialRefined[Predicate] =
    new PartialRefined[Predicate]

  /**
   * refineType[RefinedType] allows to retrieve a RefinedType (example: NonEmptyString) from a path.
   *
   * Unlike `refine[Predicate]` method, `refineType[RefinedType]`
   * allows you to a pass a fully formed refined type
   * and be careless about the `Config` of the underlying type.
   *
   * Example:
   *
   * {{{
   *
   *   type NonEmptyString = String Refined NonEmpty
   *   refineType[NonEmptyString]("USERNAME")
   *
   *   // is same as
   *
   *   val userName: Config[String] = string("USERNAME")
   *   refine[NonEmpty](userName)
   *
   * }}}
   *
   * While, `refineType[RefinedType]` is useful for simple application,
   * `refine[Predicate]` can be more flexible in complex configurations where you need more
   * orthogonality between raw config and refined configs.
   *
   * `refine[Predicate]` allows you to build entire Config without worrying
   * about `Refined` modules, allowing you to then pass the `Config[RawConfig]`
   * to `refine[Predicate]` and refine the types, which is more into an orthogonal design.
   *
   * A complete example of refineType:
   *
   * {{{
   *
   *   import zio.config._
   *   import eu.timepit.refined.types.string.NonEmptyString
   *
   *   final case class Jdbc(username: NonEmptyString, password: NonEmptyString)
   *
   *   val jdbc: Config[Jdbc] =
   *     (refineType[NonEmptyString]("username") zip refineType[NonEmptyString]("password")).to[Jdbc]
   * }}}
   */
  def refineType[R]: PartialRefinedPath[R] =
    PartialRefinedPath[R]()
}
