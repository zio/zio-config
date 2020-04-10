package zio.config

private[config] trait ConfigDocsFunctions {
  import ConfigDocs._

  final def generateDocs[K, V, A](config: ConfigDescriptor[K, V, A]): ConfigDocs[K, V] = {
    def loop[B](
      sources: Sources,
      descriptions: List[String],
      config: ConfigDescriptor[K, V, B],
      docs: ConfigDocs[K, V],
      latestPath: List[K]
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Source(source, _) =>
          Leaf(Sources(source.sourceDescription ++ sources.set), descriptions, None)

        case ConfigDescriptor.Default(c, _) =>
          loop(sources, descriptions, c, docs, latestPath)

        case ConfigDescriptor.DynamicMap(source, c) =>
          ConfigDocs.Map(
            latestPath,
            loop(Sources(source.sourceDescription ++ sources.set), descriptions, c, docs, latestPath.tail)
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
          ConfigDocs.NestedPath(path, loop(sources, descriptions, c, docs, latestPath :+ path))

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

    loop(Sources(Set.empty), Nil, config, Empty, List.empty)
  }

  def generateDocsWithValue[K, V, A](
    config: ConfigDescriptor[K, V, A],
    value: A
  ): Either[String, ConfigDocs[K, V]] =
    write(config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree[K, V],
          flattened: scala.collection.Map[Vector[K], ::[V]],
          c: ConfigDocs[K, V],
          keys: Vector[K]
        ): ConfigDocs[K, V] =
          c match {
            case Empty => Empty

            case Leaf(sources, descriptions, None) =>
              Leaf(sources, descriptions, flattened.get(keys).flatMap(_.headOption))

            case a: Leaf[V] => a

            case NestedPath(path, docs) =>
              NestedPath(path, loop(tree, flattened, docs, keys :+ path))

            case Both(left, right) =>
              Both(loop(tree, flattened, left, keys), loop(tree, flattened, right, keys))

            case OneOf(left, right) =>
              OneOf(loop(tree, flattened, left, keys), loop(tree, flattened, right, keys))

            case Map(key, docs) =>
              tree.getPath((keys :+ key).toList) match {
                case PropertyTree.Record(value) =>
                  val list =
                    value.toList.map(
                      t =>
                        Map(
                          t._1,
                          loop(
                            t._2,
                            t._2.flatten,
                            docs,
                            Vector.empty
                          )
                        )
                    )

                  Sequence(list)

                case _ => Map(key, loop(tree, flattened, docs, keys))
              }

            case Sequence(element :: Nil) =>
              tree.getPath(keys.toList) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  Sequence(value.map(t => loop(t, t.flatten, element, Vector.empty)))
                case _ => Sequence(loop(tree, flattened, element, keys) :: Nil)
              }

            case s: Sequence[K, V] => s
          }

        loop(tree, tree.flatten, generateDocs(config), Vector.empty)
      })
}
