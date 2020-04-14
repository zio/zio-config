package zio.config

private[config] trait ConfigDocsFunctions {
  import ConfigDocs._

  final def generateDocs[K, V, A](config: ConfigDescriptor[K, V, A]): ConfigDocs[K, V] = {
    def loop[B](
      sources: Set[ConfigSource.Name],
      descriptions: List[String],
      config: ConfigDescriptor[K, V, B],
      latestPath: Option[K]
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Source(source, _) =>
          Leaf((source.names ++ sources), descriptions, None)

        case ConfigDescriptor.Default(c, _) =>
          loop(sources, descriptions, c, latestPath)

        case ConfigDescriptor.DynamicMap(source, c) =>
          ConfigDocs.DynamicMap(
            loop((source.names ++ sources), descriptions, c, None)
          )

        case ConfigDescriptor.Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop((source.names ++ sources), descriptions, c, latestPath)
          )

        case ConfigDescriptor.Describe(c, desc) =>
          loop(sources, desc :: descriptions, c, latestPath)

        case ConfigDescriptor.Optional(c) =>
          loop(sources, descriptions, c, latestPath)

        case ConfigDescriptor.Nested(path, c) =>
          ConfigDocs.Nested(path, loop(sources, descriptions, c, Some(path)))

        case ConfigDescriptor.XmapEither(c, _, _) =>
          loop(sources, descriptions, c, latestPath)

        case ConfigDescriptor.Zip(left, right) =>
          ConfigDocs.Zip(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case ConfigDescriptor.OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case ConfigDescriptor.OrElse(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )
      }

    loop(Set.empty, Nil, config, None)
  }

  def generateReport[K, V, A](
    config: ConfigDescriptor[K, V, A],
    value: A
  ): Either[String, ConfigDocs[K, V]] =
    write(config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree[K, V],
          schemaDocs: ConfigDocs[K, V],
          keys: List[K]
        ): ConfigDocs[K, V] =
          schemaDocs match {
            case Leaf(sources, descriptions, None) =>
              // Feed value when it hits leaf
              tree.getPath(keys) match {
                case PropertyTree.Leaf(value) => Leaf(sources, descriptions, Some(value))
                case _                        => Leaf(sources, descriptions, None)
              }

            case a: Leaf[V] => a

            case Nested(path, docs) =>
              Nested(path, loop(tree, docs, keys :+ path))

            case Zip(left, right) =>
              Zip(loop(tree, left, keys), loop(tree, right, keys))

            case OrElse(left, right) =>
              OrElse(loop(tree, left, keys), loop(tree, right, keys))

            case cd: DynamicMap[K, V] =>
              tree.getPath(keys) match {
                case rec: PropertyTree.Record[K, V] =>
                  DynamicMap(cd.schemaDocs, rec.value.toList.map { keyTree =>
                    keyTree._1 -> loop(keyTree._2, cd.schemaDocs, List.empty)
                  }.toMap)
                case v => DynamicMap(loop(v, cd.schemaDocs, keys), Map.empty[K, ConfigDocs[K, V]])
              }

            case Sequence(schema, values) =>
              tree.getPath(keys) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  Sequence(schema, value.map(t => loop(t, schema, List.empty)))
                case _ => Sequence(schema, loop(tree, schema, keys) :: values)
              }
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
