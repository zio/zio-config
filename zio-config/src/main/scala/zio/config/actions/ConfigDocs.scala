package zio.config.actions

import zio.config.ConfigDescriptor

sealed trait ConfigDocs[K, V]

object ConfigDocs {

  final case class Empty[K, V]()                                                                extends ConfigDocs[K, V]
  final case class PathDetails[K, V](path: K, value: Option[V], docs: List[String])             extends ConfigDocs[K, V]
  final case class Nested[K, V](path: K, docs: ConfigDocs[K, V])                                extends ConfigDocs[K, V]
  final case class And[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])                   extends ConfigDocs[K, V]
  final case class Or[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])                    extends ConfigDocs[K, V]

  final def createDoc[K, V, A](config: ConfigDescriptor[K, V, A], value: Option[A]): ConfigDocs[K, V] = {
    def loop[B](
      descAcc: List[String],
      config: ConfigDescriptor[K, V, B],
      docs: ConfigDocs[K, V],
      configValue: Option[B],
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Empty() => docs
        case ConfigDescriptor.Source(path, p, source) =>
          PathDetails(
            path,
            configValue.map(t => p.write(t)),
            source.sourceDescription ++ descAcc
          )
        case ConfigDescriptor.Default(c, _) =>
          loop(descAcc, c, docs, configValue)

        case ConfigDescriptor.Describe(c, description) =>
          loop(description :: descAcc, c, docs, configValue)

        case ConfigDescriptor.Optional(c) =>
          configValue match {
            case Some(result) =>
              result.fold(loop(descAcc, c, docs, None))(v => loop(descAcc, c, docs, Some(v)))
            case None =>
              loop(descAcc, c, docs, None)
          }

        case ConfigDescriptor.Nested(c, path) =>
          ConfigDocs.Nested(path, loop(descAcc, c, docs, value))

        case ConfigDescriptor.XmapEither(c, _, to) =>
          configValue match {
            case Some(v) =>
              to(v).fold(_ => loop(descAcc, c, docs, None), vv => loop(descAcc, c, docs, Some(vv)))
            case None =>
              loop(descAcc, c, docs, None)
          }

        case ConfigDescriptor.Zip(left, right) =>
          configValue match {
            case Some(tuple) =>
              ConfigDocs.And(
                loop(descAcc, left, docs, Some(tuple._1)),
                loop(descAcc, right, docs, Some(tuple._2))
              )

            case None =>
              ConfigDocs.And(loop(descAcc, left, docs, None), loop(descAcc, right, docs, None))
          }

        case ConfigDescriptor.OrElseEither(left, right) =>
          configValue match {
            case Some(res) =>
              res match {
                case Left(vv) =>
                  ConfigDocs.Or(loop(descAcc, left, docs, Some(vv)), loop(descAcc, right, docs, None))

                case Right(vv) =>
                  ConfigDocs.Or(loop(descAcc, left, docs, None), loop(descAcc, right, docs, Some(vv)))
              }

            case None =>
              ConfigDocs.Or(loop(descAcc, left, docs, None), loop(descAcc, right, docs, None))
          }
      }

    loop(Nil, config, Empty(), value)
  }
}
