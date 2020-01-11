package zio.config

private[config] trait ConfigDocsFunctions {
  import ConfigDocs._
  import ConfigDocs.Details._

  final def generateDocs[K, V, A](config: ConfigDescriptor[K, V, A]): ConfigDocs[K, V] = {
    def loop[B](
      descAcc: Descriptions,
      config: ConfigDescriptor[K, V, B],
      docs: ConfigDocs[K, V]
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Source(path, source, _) =>
          Path(
            path,
            Descriptions(Sources(source.sourceDescription ++ descAcc.sources.list), descAcc.descriptions)
          )

        case ConfigDescriptor.Default(c, _) =>
          loop(descAcc, c, docs)

        case ConfigDescriptor.Sequence(c) =>
          loop(descAcc.copy(descriptions = "value of type list" :: descAcc.descriptions), c, docs)

        case ConfigDescriptor.Describe(c, description) =>
          loop(descAcc.copy(descriptions = description :: descAcc.descriptions), c, docs)

        case ConfigDescriptor.Optional(c) =>
          loop(descAcc, c, docs)

        case ConfigDescriptor.Nested(path, c) =>
          ConfigDocs.NestedPath(path, loop(descAcc, c, docs))

        case ConfigDescriptor.XmapEither(c, _, _) =>
          loop(descAcc, c, docs)

        case ConfigDescriptor.Zip(left, right) =>
          ConfigDocs.Both(
            loop(descAcc, left, docs),
            loop(descAcc, right, docs)
          )

        case ConfigDescriptor.OrElseEither(left, right) =>
          ConfigDocs.OneOf(
            loop(descAcc, left, docs),
            loop(descAcc, right, docs)
          )

        case ConfigDescriptor.OrElse(left, right) =>
          ConfigDocs.OneOf(
            loop(descAcc, left, docs),
            loop(descAcc, right, docs)
          )
      }

    loop(Descriptions(Sources(Nil), Nil), config, Empty)
  }

  def generateDocsWithValue[K, V, A](
    config: ConfigDescriptor[K, V, A],
    value: A
  ): Either[String, ConfigDocs[K, V]] =
    write(config, value)
      .map(tree => {
        val flattened = tree.flatten

        def loop(c: ConfigDocs[K, V], initialValue: Vector[K]): ConfigDocs[K, V] =
          c match {
            case Empty => Empty

            case Path(path, docs) =>
              val updated: Details[V] =
                docs match {
                  case Descriptions(sources, descriptions) =>
                    DescriptionsWithValue(flattened.get(initialValue :+ path).map(_.head), sources, descriptions)
                  case a => a
                }
              Path(path, updated)

            case NestedPath(path, docs) =>
              NestedPath(path, loop(docs, initialValue :+ path))

            case Both(left, right) =>
              Both(loop(left, initialValue), loop(right, initialValue))

            case OneOf(left, right) =>
              OneOf(loop(left, initialValue), loop(right, initialValue))
          }

        loop(generateDocs(config), Vector.empty)
      })
}
