package zio.config.refined

import eu.timepit.refined.api.{RefType, Validate}
import zio.Config
import zio.config.magnolia.DeriveConfig

final case class PartialRefinedPath[FAP]() {
  def apply[F[_, _], A, P](
    path: String
  )(implicit
    ev: F[A, P] =:= FAP,
    rt: RefType[F],
    v: Validate[A, P],
    d: DeriveConfig[A]
  ): Config[F[A, P]] =
    d.desc
      .nested(path)
      .mapOrFail[F[A, P]](t =>
        rt.refine[P](t)
          .swap
          .map(str => Config.Error.InvalidData(message = str))
          .swap
          .map(_.asInstanceOf[F[A, P]]) // =:= ev.apply doesn't work with 2.12.13
      )

}
