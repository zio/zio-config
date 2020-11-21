package zio.config

import eu.timepit.refined.api.{ RefType, Refined, Validate }
import zio.config.magnolia.DeriveConfigDescriptor.Descriptor
import zio.config.ConfigDescriptor._

package object refined {
  /**
   * Support to retrieve `refined` types from ConfigDescriptor
   * {{{
   *   import eu.timepit.refined.string._
   *
   *   val configDescriptor: ConfigDescriptor[Refined[String, Uuid]] =
   *     refined[String, Uuid]("ID")
   * }}}
   */
  def refined[A, P](path: String)(
    implicit desc: Descriptor[A],
    validate: Validate[A, P]
  ): ConfigDescriptor[Refined[A, P]] =
    nested(path)(desc.desc).transformOrFail[Refined[A, P]](
      RefType.applyRef[Refined[A, P]](_),
      rf => Right(rf.value)
    )
}
