package zio.config

import com.github.ghik.silencer.silent
import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.config.ConfigDescriptor.nested
import zio.config.magnolia.Descriptor

package object refined {

  /**
   * FIXME
   * Automatically derive instances of Descriptor for any refined types
   */
  @silent("deprecated")
  implicit def deriveRefinedDescriptor[A, P](implicit
    desc: Descriptor[A],
    validate: Validate[A, P]
  ): Descriptor[Refined[A, P]] =
    Descriptor(refine[P](desc.desc))

  /**
   * `refine` allows us to retrieve a `refined` type from a given path.
   *
   * Example:
   *
   * {{{
   *   import eu.timepit.refined.string._
   *
   *   val configDescriptor: ConfigDescriptor[Refined[String, Uuid]] =
   *     refined[String, Uuid]("ID")
   * }}}
   */
  @silent("deprecated")
  def refine[A, P](path: String)(implicit
    desc: Descriptor[A],
    validate: Validate[A, P]
  ): ConfigDescriptor[Refined[A, P]] =
    nested(path)(desc.desc)
      .transformOrFail[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )

  /**
   * refine[Predicate] allows to retrieve a `refined` type given an existing `ConfigDescriptor`
   * and a predicate Predicate. Example of a Predicate is `NonEmpty`.
   *
   * Example:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   final case class MyConfig(url: String, port: Int)
   *
   *   val configs: ConfigDescriptor[List[MyConfig]] =
   *     list("databases")(descriptor[MyConfig])
   *
   *   val configDescriptor: ConfigDescriptor[Refined[List[MyConfig], Size[Greater[W.`2`.T]]]] =
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
   * and be careless about the `ConfigDescriptor` of the underlying type.
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
   *   val userName: ConfigDescriptor[String] = string("USERNAME")
   *   refine[NonEmpty](userName)
   *
   * }}}
   *
   * While, `refineType[RefinedType]` is useful for simple application,
   * `refine[Predicate]` can be more flexible in complex configurations where you need more
   * orthogonality between raw config and refined configs.
   *
   * `refine[Predicate]` allows you to build entire ConfigDescriptor without worrying
   * about `Refined` modules, allowing you to then pass the `ConfigDescriptor[RawConfig]`
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
   *   val jdbc: ConfigDescriptor[Jdbc] =
   *     (refineType[NonEmptyString]("username") zip refineType[NonEmptyString]("password")).to[Jdbc]
   * }}}
   */
  def refineType[R]: PartialRefinedPath[R] =
    PartialRefinedPath[R]()
}
