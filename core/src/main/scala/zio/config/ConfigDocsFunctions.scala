package zio.config

private[config] trait ConfigDocsFunctions {
  import ConfigDocs._

  final def generateDocs[A](config: ConfigDescriptor[A]): ConfigDocs = {
    def loop[B](
      sources: Sources,
      descriptions: List[String],
      config: ConfigDescriptor[B],
      docs: ConfigDocs
    ): ConfigDocs =
      config match {
        case ConfigDescriptor.Source(source, _) =>
          Leaf(Sources(source.sourceDescription ++ sources.set), descriptions, None)

        case ConfigDescriptor.Default(c, _) =>
          loop(sources, descriptions, c, docs)

        case ConfigDescriptor.Sequence(source, c) =>
          ConfigDocs.Sequence(loop(Sources(source.sourceDescription ++ sources.set), descriptions, c, docs) :: Nil)

        case ConfigDescriptor.Describe(c, desc) =>
          loop(sources, desc :: descriptions, c, docs)

        case ConfigDescriptor.Optional(c) =>
          loop(sources, descriptions, c, docs)

        case ConfigDescriptor.Nested(path, c) =>
          ConfigDocs.NestedPath(path, loop(sources, descriptions, c, docs))

        case ConfigDescriptor.XmapEither(c, _, _) =>
          loop(sources, descriptions, c, docs)

        case ConfigDescriptor.Zip(left, right) =>
          ConfigDocs.Both(
            loop(sources, descriptions, left, docs),
            loop(sources, descriptions, right, docs)
          )

        case ConfigDescriptor.OrElseEither(left, right) =>
          ConfigDocs.OneOf(
            loop(sources, descriptions, left, docs),
            loop(sources, descriptions, right, docs)
          )

        case ConfigDescriptor.OrElse(left, right) =>
          ConfigDocs.OneOf(
            loop(sources, descriptions, left, docs),
            loop(sources, descriptions, right, docs)
          )
      }

    loop(Sources(Set.empty), Nil, config, Empty)
  }

  def generateDocsWithValue[A](
    config: ConfigDescriptor[A],
    value: A
  ): Either[String, ConfigDocs] =
    write(config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree,
          flattened: Map[Vector[String], ::[String]],
          c: ConfigDocs,
          initialValue: Vector[String]
        ): ConfigDocs =
          c match {
            case Empty => Empty

            case Leaf(sources, descriptions, None) =>
              Leaf(sources, descriptions, flattened.get(initialValue).flatMap(_.headOption))

            case a: Leaf[v] => a

            case NestedPath(path, docs) =>
              NestedPath(path, loop(tree, flattened, docs, initialValue :+ path))

            case Both(left, right) =>
              Both(loop(tree, flattened, left, initialValue), loop(tree, flattened, right, initialValue))

            case OneOf(left, right) =>
              OneOf(loop(tree, flattened, left, initialValue), loop(tree, flattened, right, initialValue))

            case Sequence(element :: Nil) =>
              tree.getPath(initialValue.toList) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  Sequence(value.map(t => loop(t, t.flatten, element, Vector.empty)))
                case _ => Sequence(loop(tree, flattened, element, initialValue) :: Nil)
              }

            case s: Sequence => s
          }

        loop(tree, tree.flatten, generateDocs(config), Vector.empty)
      })
}
