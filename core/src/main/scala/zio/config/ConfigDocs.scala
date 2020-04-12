package zio.config

import zio.config

sealed trait ConfigDocs[+K, +V]
object ConfigDocs {
  final case class Leaf[V](sources: Sources, descriptions: List[String], value: Option[V] = None)
      extends ConfigDocs[Nothing, V]

  final case class Sources(set: Set[String])

  final case object Empty                                                       extends ConfigDocs[Nothing, Nothing]
  final case class NestedPath[K, V](path: K, docs: ConfigDocs[K, V])            extends ConfigDocs[K, V]
  final case class Both[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V])  extends ConfigDocs[K, V]
  final case class OneOf[K, V](left: ConfigDocs[K, V], right: ConfigDocs[K, V]) extends ConfigDocs[K, V]
  final case class Sequence[K, V](element: List[ConfigDocs[K, V]])              extends ConfigDocs[K, V]
  final case class DynamicMapInit[K, V](element: config.ConfigDocs[K, V])       extends ConfigDocs[K, V]
  // During reporting, we know the dynamic keys; Fixme; Make it better orthogonal
  final case class DynamicMap[K, V](element: Map[K, config.ConfigDocs[K, V]]) extends ConfigDocs[K, V]

}
