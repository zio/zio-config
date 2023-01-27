package zio.config.refined

import eu.timepit.refined.api.{RefType, Validate}
import zio.Config
import zio.config.magnolia.Descriptor

import Config.nested

final case class PartialRefinedPath[FAP]() {
  def apply[F[_, _], A, P](
    path: String
  )(implicit
    ev: F[A, P] =:= FAP,
    rt: RefType[F],
    v: Validate[A, P],
    d: Descriptor[A]
  ): Config[F[A, P]] =
    nested(path)(
      d.desc
        .transformOrFail[F[A, P]](
          t => rt.refine[P](t).map(_.asInstanceOf[F[A, P]]), // =:= ev.apply doesn't work with 2.12.13
          rf => Right(rt.unwrap(rf))
        )
    )
}
