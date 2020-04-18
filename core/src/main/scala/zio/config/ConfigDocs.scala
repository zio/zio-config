package zio.config

sealed trait ConfigDocs[+K, +V]
object ConfigDocs {
  final case class Leaf[V](sources: Set[ConfigSource.Name], descriptions: List[String], value: Option[V] = None)
      extends ConfigDocs[Nothing, V]
  final case class Nested[K, V](path: K, docs: ConfigDocs[K, V])                         extends ConfigDocs[K, V]
  final case class Zip[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])            extends ConfigDocs[K, V]
  final case class OrElse[K, V](leftDocs: ConfigDocs[K, V], rightDocs: ConfigDocs[K, V]) extends ConfigDocs[K, V]
  final case class Sequence[K, V](schemaDocs: ConfigDocs[K, V], valueDocs: List[ConfigDocs[K, V]] = List.empty)
      extends ConfigDocs[K, V]
  final case class DynamicMap[K, V](
    schemaDocs: ConfigDocs[K, V],
    valueDocs: Map[K, ConfigDocs[K, V]] = Map.empty[K, ConfigDocs[K, V]]
  ) extends ConfigDocs[K, V]
}
