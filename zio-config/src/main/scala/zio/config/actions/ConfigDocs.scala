package zio.config.actions

import zio.config.ConfigDescriptor

sealed trait ConfigDocs

object ConfigDocs {

  final case class Empty()                                  extends ConfigDocs
  final case class Leaf(value: KeyDescription)              extends ConfigDocs
  final case class And(left: ConfigDocs, right: ConfigDocs) extends ConfigDocs
  final case class Or(left: ConfigDocs, right: ConfigDocs)  extends ConfigDocs

  final case class KeyDescription(path: String, value: Option[String], docs: List[String])

  final def createDoc[A](config: ConfigDescriptor[A], value: Option[A]): ConfigDocs = {
    def loop[B](
      acc: List[String],
      desc: String,
      config: ConfigDescriptor[B],
      docs: ConfigDocs,
      value: Option[B]
    ): ConfigDocs =
      config match {
        case ConfigDescriptor.Empty() => docs
        case ConfigDescriptor.Source(path, p) =>
          ConfigDocs.Leaf(
            KeyDescription(
              path,
              value.map(t => p.write(t)),
              if (desc.isEmpty) acc
              else desc :: acc
            )
          )
        case ConfigDescriptor.Default(c, _) =>
          loop(acc, desc, c, docs, value)

        case ConfigDescriptor.Describe(c, message) =>
          loop(message :: acc, desc, c, docs, value)

        case ConfigDescriptor.Optional(c) =>
          value match {
            case Some(result) =>
              result.fold(loop(acc, desc, c, docs, None))(v => loop(acc, desc, c, docs, Some(v)))
            case None =>
              loop(acc, desc, c, docs, None)
          }

        case ConfigDescriptor.XmapEither(c, _, to) =>
          value match {
            case Some(v) =>
              to(v).fold(_ => loop(acc, desc, c, docs, None), vv => loop(acc, desc, c, docs, Some(vv)))
            case None =>
              loop(acc, desc, c, docs, None)
          }

        case ConfigDescriptor.Zip(left, right) =>
          value match {
            case Some(tuple) =>
              ConfigDocs.And(loop(acc, desc, left, docs, Some(tuple._1)), loop(acc, desc, right, docs, Some(tuple._2)))

            case None =>
              ConfigDocs.And(loop(acc, desc, left, docs, None), loop(acc, desc, right, docs, None))
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          value match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs.Or(loop(acc, desc, left, docs, Some(vv)), loop(acc, desc, right, docs, None))

                case Right(vv) =>
                  ConfigDocs.Or(loop(acc, desc, left, docs, None), loop(acc, desc, right, docs, Some(vv)))
              }

            case None =>
              ConfigDocs.Or(loop(acc, desc, left, docs, None), loop(acc, desc, right, docs, None))
          }
      }

    loop(Nil, "", config, Empty(), value)
  }
}
