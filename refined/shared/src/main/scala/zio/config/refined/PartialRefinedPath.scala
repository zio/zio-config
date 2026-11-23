package zio.config.refined

import eu.timepit.refined.api.{RefType, Validate}
import zio.Config
import zio.config.magnolia.DeriveConfig

/**
 * The value that `refineType[FAP]` returns. Its `apply(path)` reads the refined type `FAP`, such as `NonEmptyString`,
 * from the key `path`.
 */
final case class PartialRefinedPath[FAP]() {

  /**
   * Reads the value at `path` with the `DeriveConfig[A]` of the underlying type, and refines it with the predicate `P`,
   * where `F[A, P]` is the refined type `FAP`. A value that fails the predicate fails with `Config.Error.InvalidData`,
   * whose message is refined's message and whose path is empty.
   *
   * {{{
   * import zio._
   * import eu.timepit.refined.types.string.NonEmptyString
   *
   * val username: Config[NonEmptyString] = PartialRefinedPath[NonEmptyString]().apply("USERNAME")
   *
   * val loaded: IO[Config.Error, NonEmptyString] = ConfigProvider.fromMap(Map("USERNAME" -> "ada")).load(username)
   * // succeeds with ada
   * }}}
   */
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
