package zio.config

import com.github.ghik.silencer.silent
import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.config.ConfigDescriptor.nested
import zio.config.magnolia.DeriveConfigDescriptor.Descriptor

package object refined {

  /**
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
   * refine allows to retrieve a `refined` type given an existing `ConfigDescriptor`
   *
   * Example:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(url: String, port: Int)
   *
   *   val configs: ConfigDescriptor[List[MyConfig]] =
   *     list("databases")(descriptor[MyConfig])
   *
   *   val configDescriptor: ConfigDescriptor[Refined[List[MyConfig], Size[Greater[W.`2`.T]]]] =
   *     refined[Size[Greater[W.`2`.T]]](configs)
   * }}}
   */
  def refine[P]: PartialRefined[P] =
    new PartialRefined[P]
}
