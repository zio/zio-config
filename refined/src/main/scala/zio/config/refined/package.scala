package zio.config

import eu.timepit.refined.api.{ RefType, Refined, Validate }
import zio.config.ConfigDescriptor._

package object refined
    extends NumericSupport
    with StringSupport
    with CharSupport
    with BooleanSupport
    with CollectionSupport {

  /** Add support for custom predicates */
  def asRefined[A, P](
    desc: ConfigDescriptor[A]
  )(
    implicit ev: Validate[A, P]
  ): ConfigDescriptor[Refined[A, P]] =
    desc
      .xmapEither[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )
}
