package zio.config

import eu.timepit.refined.api.{ RefType, Refined, Validate }

package object refined
    extends NumericSupport
    with StringSupport
    with CharSupport
    with BooleanSupport
    with CollectionSupport {

  /** Add support for custom predicates */
  def asRefined[K, V, A, P](
    desc: ConfigDescriptor[K, V, A]
  )(
    implicit ev: Validate[A, P]
  ): ConfigDescriptor[K, V, Refined[A, P]] =
    desc
      .xmapEither[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_)
      )(
        rf => Right(rf.value)
      )

}
