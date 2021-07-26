package zio.config.refined

import eu.timepit.refined.api.{RefType, Validate}
import zio.config.ConfigDescriptor
import zio.config.magnolia.Descriptor

import ConfigDescriptor.nested

final class PartialRefinedPath[FAP] {
  def apply[F[_, _], A, P](
    path: String
  )(implicit
    ev: FAP =:= F[A, P],
    rt: RefType[F],
    v: Validate[A, P],
    d: Descriptor[A]
  ): ConfigDescriptor[F[A, P]] =
    nested(path)(
      d.desc
        .transformOrFail[F[A, P]](
          t => rt.refine[P](t).map(ev.flip.apply),
          rf => Right(rt.unwrap(rf))
        )
    )
}
