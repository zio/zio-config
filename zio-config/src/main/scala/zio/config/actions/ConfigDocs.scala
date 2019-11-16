package zio.config.actions

import zio.config.{ ConfigDescriptor }

sealed trait ConfigDocs[+K, +V]

object ConfigDocs {
  sealed trait ConfigValueDetail[+V]

  case class Descriptions[V](descriptions: List[String])                       extends ConfigValueDetail[V]
  case class DescriptionsWithValue[V](value: Option[V], docs: Descriptions[V]) extends ConfigValueDetail[V]

  final case class Empty[K, V]()                                              extends ConfigDocs[K, V]
  final case class PathDetails[K, V](path: K, docs: ConfigValueDetail[V])     extends ConfigDocs[K, V]
  final case class NestedConfig[K, V](path: K, docs: ConfigDocs[K, V])        extends ConfigDocs[K, V]
  final case class And[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V]) extends ConfigDocs[K, V]
  final case class Or[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])  extends ConfigDocs[K, V]

  final def createDoc[K, V, A](config: ConfigDescriptor[K, V, A]): ConfigDocs[K, V] = {
    def loop[B](
      descAcc: Descriptions[V],
      config: ConfigDescriptor[K, V, B],
      docs: ConfigDocs[K, V]
    ): ConfigDocs[K, V] =
      config match {
        case ConfigDescriptor.Empty() => docs
        case ConfigDescriptor.Source(path, source, p) =>
          PathDetails(
            path,
            Descriptions(source.sourceDescription ++ descAcc.descriptions)
          )
        case ConfigDescriptor.Default(c, _) =>
          loop(descAcc, c, docs)

        case ConfigDescriptor.Describe(c, description) =>
          loop(Descriptions(description :: descAcc.descriptions), c, docs)

        case ConfigDescriptor.Optional(c) =>
          loop(descAcc, c, docs)

        // intentional duplication of pattern matching to get over the type issues.
        case a @ ConfigDescriptor.Nested(c, path) =>
          a match {
            case ConfigDescriptor.Nested(c, path) => ConfigDocs.NestedConfig(path, loop(descAcc, c, docs))
          }

        case ConfigDescriptor.XmapEither(c, _, to) =>
          loop(descAcc, c, docs)

        case ConfigDescriptor.Zip(left, right) =>
          ConfigDocs.And(
            loop(descAcc, left, docs),
            loop(descAcc, right, docs)
          )

        case ConfigDescriptor.OrElseEither(left, right) =>
          ConfigDocs.Or(
            loop(descAcc, left, docs),
            loop(descAcc, right, docs)
          )
      }

    loop(Descriptions(Nil), config, Empty())
  }

  def createDocWithValues[K, V, A](config: ConfigDescriptor[K, V, A], value: A): Either[String, ConfigDocs[K, V]] = {
    val propertyTree = Write.write(config, value)

    propertyTree.map(tree => {
      val flattened = tree.flatten

      def loop(c: ConfigDocs[K, V], initialValue: Vector[K]): ConfigDocs[K, V] =
        c match {
          case Empty() => Empty()
          case PathDetails(path, docs) =>
            val updated: ConfigValueDetail[V] =
              docs match {
                case Descriptions(descriptions) =>
                  DescriptionsWithValue(flattened.get(initialValue :+ path), Descriptions(descriptions))
                case a => a
              }
            PathDetails(path, updated)

          case NestedConfig(path, docs) =>
            NestedConfig(path, loop(docs, initialValue :+ path))
          case And(left, right) => And(loop(left, initialValue), loop(right, initialValue))
          case Or(left, right)  => Or(loop(left, initialValue), loop(right, initialValue))
        }

      loop(createDoc(config), Vector.empty)
    })
  }
}
