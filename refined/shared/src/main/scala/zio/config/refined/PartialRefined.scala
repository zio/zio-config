package zio.config.refined

import eu.timepit.refined.api.{RefType, Refined, Validate}
import zio.config._

private[refined] class PartialRefined[P] {
  def apply[A](desc: Config[A])(implicit validate: Validate[A, P]): Config[Refined[A, P]] =
    desc
      .transformOrFail[Refined[A, P]](
        RefType.applyRef[Refined[A, P]](_),
        rf => Right(rf.value)
      )
}
