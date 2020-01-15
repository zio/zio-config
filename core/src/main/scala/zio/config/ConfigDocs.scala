package zio.config

sealed trait ConfigDocs[+K, +V]
object ConfigDocs {
  sealed trait Details[+V] {
    def sources: Sources
    def descriptions: List[String]
  }
  object Details {
    final case class Descriptions(sources: Sources, descriptions: List[String]) extends Details[Nothing]
    final case class DescriptionsWithValue[V](value: Option[V], sources: Sources, descriptions: List[String])
        extends Details[V]
  }

  final case class Sources(list: List[String])

  final case object Empty                                                       extends ConfigDocs[Nothing, Nothing]
  final case class Path[K, V](path: K, details: Details[V])                     extends ConfigDocs[K, V]
  final case class NestedPath[K, V](path: K, docs: ConfigDocs[K, V])            extends ConfigDocs[K, V]
  final case class Both[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])  extends ConfigDocs[K, V]
  final case class OneOf[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V]) extends ConfigDocs[K, V]
}
