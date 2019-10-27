package zio.config.actions

import zio.config.ConfigDescriptor

sealed trait ConfigDocs[K, V]

object ConfigDocs {

  final case object Empty                                                                       extends ConfigDocs
  final case class PathDetails[K, V](path: K, value: Option[V], docs: List[String])             extends ConfigDocs
  final case class Nested[K, V](path: K, docs: ConfigDocs[K, V])                                extends ConfigDocs[K, V]
  final case class And(left: ConfigDocs, right: ConfigDocs)                                     extends ConfigDocs
  final case class Or(left: ConfigDocs, right: ConfigDocs)                                      extends ConfigDocs

  final def createDoc[A](config: ConfigDescriptor[A], value: Option[A]): ConfigDocs = {
    def loop[B](
      descAcc: List[String],
      config: ConfigDescriptor[B],
      docs: ConfigDocs,
      configValue: Option[B],
      paths: Vector[String]
    ): ConfigDocs =
      config match {
        case ConfigDescriptor.Empty() => docs
        case ConfigDescriptor.Source(path, p) =>
          PathDetails(
            paths :+ path,
            configValue.map(t => p.write(t)),
            descAcc
          )
        case ConfigDescriptor.Default(c, _) =>
          loop(descAcc, c, docs, configValue, paths)

        case ConfigDescriptor.Describe(c, description) =>
          loop(description :: descAcc, c, docs, configValue, paths)

        case ConfigDescriptor.Optional(c) =>
          configValue match {
            case Some(result) =>
              result.fold(loop(descAcc, c, docs, None, paths))(v => loop(descAcc, c, docs, Some(v), paths))
            case None =>
              loop(descAcc, c, docs, None, paths)
          }

        case ConfigDescriptor.Nested(c, path) =>
          loop(descAcc, c, docs, configValue, paths :+ path)

        case ConfigDescriptor.XmapEither(c, _, to) =>
          configValue match {
            case Some(v) =>
              to(v).fold(_ => loop(descAcc, c, docs, None, paths), vv => loop(descAcc, c, docs, Some(vv), paths))
            case None =>
              loop(descAcc, c, docs, None, paths)
          }

        case ConfigDescriptor.Zip(left, right) =>
          configValue match {
            case Some(tuple) =>
              ConfigDocs.And(
                loop(descAcc, left, docs, Some(tuple._1), paths),
                loop(descAcc, right, docs, Some(tuple._2), paths)
              )

            case None =>
              ConfigDocs.And(loop(descAcc, left, docs, None, paths), loop(descAcc, right, docs, None, paths))
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          configValue match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs.Or(loop(descAcc, left, docs, Some(vv), paths), loop(descAcc, right, docs, None, paths))

                case Right(vv) =>
                  ConfigDocs.Or(loop(descAcc, left, docs, None, paths), loop(descAcc, right, docs, Some(vv), paths))
              }

            case None =>
              ConfigDocs.Or(loop(descAcc, left, docs, None, paths), loop(descAcc, right, docs, None, paths))
          }
      }

    loop(Nil, config, Empty(), value, Vector.empty)
  }
}
