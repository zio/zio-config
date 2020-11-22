package zio.config

import eu.timepit.refined.api.{ RefType, Refined, Validate }
import zio.config.magnolia.DeriveConfigDescriptor.Descriptor
import zio.config.ConfigDescriptor.nested

package object refined {

  /**
   * Support to retrieve `refined` type from a given path
   * given an implicit defintion of zio.config.magnolia.DeriveConfigDescriptor.Descriptor
   *
   * {{{
   *   import eu.timepit.refined.string._
   *
   *   val configDescriptor: ConfigDescriptor[Refined[String, Uuid]] =
   *     refined[String, Uuid]("ID")
   * }}}
   */
  def refine[A, P](path: String)(
    implicit desc: Descriptor[A],
    validate: Validate[A, P]
  ): ConfigDescriptor[Refined[A, P]] =
    nested(path)(desc.desc)
      .xmapEither[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )

  /**
   * Support to retrieve `refined` types from an existing ConfigDescriptor
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
   *   val configDescriptor: ConfigDescriptor[Refined[List[MyConfig], NonEmpty]] =
   *     refined[List[MyConfig], NonEmpty](configs)
   * }}}
   */
  def refine[A, P](
    desc: ConfigDescriptor[A]
  )(implicit validate: Validate[A, P]): ConfigDescriptor[Refined[A, P]] =
    desc
      .xmapEither[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )
}
