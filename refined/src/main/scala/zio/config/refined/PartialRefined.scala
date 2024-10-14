package zio.config.refined

import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.Config

private[refined] class PartialRefined[P] {
  def apply[A](desc: Config[A])(implicit validate: Validate[A, P]): Config[Refined[A, P]] =
    desc
      .mapOrFail[Refined[A, P]](v =>
        RefType.applyRef[Refined[A, P]](v).swap.map(str => Config.Error.InvalidData(message = str)).swap
      )
}
