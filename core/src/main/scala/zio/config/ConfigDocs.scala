package zio.config

sealed trait ConfigDocs
object ConfigDocs {
  final case class Leaf[V](sources: Sources, descriptions: List[String], value: Option[V] = None) extends ConfigDocs

  final case class Sources(set: Set[String])

  final case object Empty                                     extends ConfigDocs
  final case class NestedPath(path: String, docs: ConfigDocs) extends ConfigDocs
  final case class Both(left: ConfigDocs, right: ConfigDocs)  extends ConfigDocs
  final case class OneOf(left: ConfigDocs, right: ConfigDocs) extends ConfigDocs
  final case class Sequence(element: List[ConfigDocs])        extends ConfigDocs
}
