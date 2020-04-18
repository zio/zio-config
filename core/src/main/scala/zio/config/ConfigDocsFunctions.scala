package zio.config

private[config] trait ConfigDocsFunctions extends WriteFunctions {
  import ConfigDocs.{ DynamicMap => DocsMap, Leaf => DocsLeaf }

  final def generateDocs[A](config: ConfigDescriptor[A]): ConfigDocs[K, V] = {
    def loop[B](
      sources: Set[ConfigSource.Name],
      descriptions: List[String],
      config: ConfigDescriptor[B],
      latestPath: Option[K]
    ): ConfigDocs[K, V] =
      config match {
        case Source(source, _) =>
          DocsLeaf((source.names ++ sources), descriptions, None)

        case Default(c, _) =>
          loop(sources, descriptions, c, latestPath)

        case cd: DynamicMap[_] =>
          ConfigDocs.DynamicMap[K, V](
            loop((cd.source.names ++ sources), descriptions, cd.config, None)
          )

        case Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop((source.names ++ sources), descriptions, c, latestPath)
          )

        case Describe(c, desc) =>
          loop(sources, desc :: descriptions, c, latestPath)

        case Optional(c) =>
          loop(sources, descriptions, c, latestPath)

        case Nested(path, c) =>
          ConfigDocs.Nested(path, loop(sources, descriptions, c, Some(path)))

        case XmapEither(c, _, _) =>
          loop(sources, descriptions, c, latestPath)

        case Zip(left, right) =>
          ConfigDocs.Zip(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case OrElse(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )
      }

    loop(Set.empty, Nil, config, None)
  }

  def generateReport[A](
    config: ConfigDescriptor[A],
    value: A
  ): Either[String, ConfigDocs[K, V]] =
    write[A](config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree[K, V],
          schemaDocs: ConfigDocs[K, V],
          keys: List[K]
        ): ConfigDocs[K, V] =
          schemaDocs match {
            case DocsLeaf(sources, descriptions, None) =>
              // Feed value when it hits leaf
              tree.getPath(keys) match {
                case PropertyTree.Leaf(value) => DocsLeaf(sources, descriptions, Some(value))
                case _                        => DocsLeaf(sources, descriptions, None)
              }

            case a: DocsLeaf[V] => a

            case ConfigDocs.Nested(path, docs) =>
              ConfigDocs.Nested(path, loop(tree, docs, keys :+ path))

            case ConfigDocs.Zip(left, right) =>
              ConfigDocs.Zip(loop(tree, left, keys), loop(tree, right, keys))

            case ConfigDocs.OrElse(left, right) =>
              ConfigDocs.OrElse(loop(tree, left, keys), loop(tree, right, keys))

            case cd: DocsMap[K, V] =>
              tree.getPath(keys) match {
                case rec: PropertyTree.Record[K, V] =>
                  DocsMap(cd.schemaDocs, rec.value.toList.map { keyTree =>
                    keyTree._1 -> loop(keyTree._2, cd.schemaDocs, List.empty)
                  }.toMap)
                case v => DocsMap(loop(v, cd.schemaDocs, keys), Map.empty[K, ConfigDocs[K, V]])
              }

            case ConfigDocs.Sequence(schema, values) =>
              tree.getPath(keys) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  ConfigDocs.Sequence(schema, value.map(t => loop(t, schema, List.empty)))
                case _ => ConfigDocs.Sequence(schema, loop(tree, schema, keys) :: values)
              }
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
