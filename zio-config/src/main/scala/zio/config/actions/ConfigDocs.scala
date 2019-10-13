package zio.config.actions

import zio.config.ConfigDescriptor

sealed trait ConfigDocs

object ConfigDocs {

  final case class Empty()                                                                      extends ConfigDocs
  final case class PathDetails(path: Vector[String], value: Option[String], docs: List[String]) extends ConfigDocs
  final case class And(left: ConfigDocs, right: ConfigDocs)                                     extends ConfigDocs
  final case class Or(left: ConfigDocs, right: ConfigDocs)                                      extends ConfigDocs

  final def createDoc[A](config: ConfigDescriptor[A], value: Option[A]): ConfigDocs = {
    def loop[B](
      acc: List[String],
      desc: String,
      config: ConfigDescriptor[B],
      docs: ConfigDocs,
      value: Option[B],
      paths: Vector[String]
    ): ConfigDocs =
      config match {
        case ConfigDescriptor.Empty() => docs
        case ConfigDescriptor.Source(path, p) =>
          PathDetails(
            paths :+ path,
            value.map(t => p.write(t)),
            if (desc.isEmpty) acc
            else desc :: acc
          )
        case ConfigDescriptor.Default(c, _) =>
          loop(acc, desc, c, docs, value, paths)

        case ConfigDescriptor.Describe(c, message) =>
          loop(message :: acc, desc, c, docs, value, paths)

        case ConfigDescriptor.Optional(c) =>
          value match {
            case Some(result) =>
              result.fold(loop(acc, desc, c, docs, None, paths))(v => loop(acc, desc, c, docs, Some(v), paths))
            case None =>
              loop(acc, desc, c, docs, None, paths)
          }

        case ConfigDescriptor.Nested(c, path) =>
          loop(acc, desc, c, docs, value, paths :+ path)

        case ConfigDescriptor.XmapEither(c, _, to) =>
          value match {
            case Some(v) =>
              to(v).fold(_ => loop(acc, desc, c, docs, None, paths), vv => loop(acc, desc, c, docs, Some(vv), paths))
            case None =>
              loop(acc, desc, c, docs, None, paths)
          }

        case ConfigDescriptor.Zip(left, right) =>
          value match {
            case Some(tuple) =>
              ConfigDocs.And(
                loop(acc, desc, left, docs, Some(tuple._1), paths),
                loop(acc, desc, right, docs, Some(tuple._2), paths)
              )

            case None =>
              ConfigDocs.And(loop(acc, desc, left, docs, None, paths), loop(acc, desc, right, docs, None, paths))
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          value match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs.Or(loop(acc, desc, left, docs, Some(vv), paths), loop(acc, desc, right, docs, None, paths))

                case Right(vv) =>
                  ConfigDocs.Or(loop(acc, desc, left, docs, None, paths), loop(acc, desc, right, docs, Some(vv), paths))
              }

            case None =>
              ConfigDocs.Or(loop(acc, desc, left, docs, None, paths), loop(acc, desc, right, docs, None, paths))
          }
      }

    loop(Nil, "", config, Empty(), value, Vector.empty)
  }
}
