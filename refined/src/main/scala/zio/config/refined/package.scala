package zio.config

import eu.timepit.refined.api.{ RefType, Refined, Validate }

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
      .transformEither[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )
}
