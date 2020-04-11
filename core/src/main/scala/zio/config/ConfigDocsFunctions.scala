package zio.config

private[config] trait ConfigDocsFunctions {
  import ConfigDocs.{ DynamicMap => DocsMap, _ }

  final def generateDocs[K, V, A](config: ConfigDescriptor[K, V, A]): ConfigDocs[K, V] = {
    def loop[B](
      sources: Sources,
      descriptions: List[String],
      config: ConfigDescriptor[K, V, B],
      docs: ConfigDocs[K, V],
      latestPath: Option[K]
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Source(source, _) =>
          Leaf(Sources(source.sourceDescription ++ sources.set), descriptions, None)

        case ConfigDescriptor.Default(c, _) =>
          loop(sources, descriptions, c, docs, latestPath)

        case ConfigDescriptor.DynamicMap(source, c) =>
          ConfigDocs.DynamicMap(
            latestPath.fold(Map.empty[K, ConfigDocs[K, V]]) { latestPath =>
              Map(latestPath -> loop(Sources(source.sourceDescription ++ sources.set), descriptions, c, docs, None))
            }
          )

        case ConfigDescriptor.Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop(Sources(source.sourceDescription ++ sources.set), descriptions, c, docs, latestPath) :: Nil
          )

        case ConfigDescriptor.Describe(c, desc) =>
          loop(sources, desc :: descriptions, c, docs, latestPath)

        case ConfigDescriptor.Optional(c) =>
          loop(sources, descriptions, c, docs, latestPath)

        case ConfigDescriptor.Nested(path, c) =>
          ConfigDocs.NestedPath(path, loop(sources, descriptions, c, docs, Some(path)))

        case ConfigDescriptor.XmapEither(c, _, _) =>
          loop(sources, descriptions, c, docs, latestPath)

        case ConfigDescriptor.Zip(left, right) =>
          ConfigDocs.Both(
            loop(sources, descriptions, left, docs, latestPath),
            loop(sources, descriptions, right, docs, latestPath)
          )

        case ConfigDescriptor.OrElseEither(left, right) =>
          ConfigDocs.OneOf(
            loop(sources, descriptions, left, docs, latestPath),
            loop(sources, descriptions, right, docs, latestPath)
          )

        case ConfigDescriptor.OrElse(left, right) =>
          ConfigDocs.OneOf(
            loop(sources, descriptions, left, docs, latestPath),
            loop(sources, descriptions, right, docs, latestPath)
          )
      }

    loop(Sources(Set.empty), Nil, config, Empty, None)
  }

  def generateDocsWithValue[K, V, A](
    config: ConfigDescriptor[K, V, A],
    value: A
  ): Either[String, ConfigDocs[K, V]] =
    write(config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree[K, V],
          c: ConfigDocs[K, V],
          keys: List[K]
        ): ConfigDocs[K, V] =
          c match {
            case Empty => Empty

            case Leaf(sources, descriptions, None) =>
              // Feed value when it hits leaf
              tree.getPath(keys) match {
                case PropertyTree.Leaf(value) => Leaf(sources, descriptions, Some(value))
                case _                        => Leaf(sources, descriptions, None)
              }

            case a: Leaf[V] => a

            case NestedPath(path, docs) =>
              NestedPath(path, loop(tree, docs, keys :+ path))

            case Both(left, right) =>
              Both(loop(tree, left, keys), loop(tree, right, keys))

            case OneOf(left, right) =>
              OneOf(loop(tree, left, keys), loop(tree, right, keys))

            case cd: DocsMap[K, V] =>
              DocsMap(cd.element.toList.map {
                case (k, value) =>
                  k -> loop(tree.getPath(keys :+ k), value, keys :+ k)
              }.toMap)

            case Sequence(element :: Nil) =>
              tree.getPath(keys) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  Sequence(value.map(t => loop(t, element, List.empty)))
                case _ => Sequence(loop(tree, element, keys) :: Nil)
              }

            case s: Sequence[K, V] => s
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
