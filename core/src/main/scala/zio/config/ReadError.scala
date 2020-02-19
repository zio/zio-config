package zio.config

sealed trait ReadError[+K]

object ReadError {
  final case class MissingValue[K](key: K, positions: Option[Int] = None)            extends ReadError[K]
  final case class ParseError[K, V](key: K, message: String)                         extends ReadError[K]
  final case class Unknown[K](key: K, cause: Throwable)                              extends ReadError[K]
  final case class OrErrors[K](leftErrors: ReadError[K], rightErrors: ReadError[K])  extends ReadError[K]
  final case class AndErrors[K](leftErrors: ReadError[K], rightErrors: ReadError[K]) extends ReadError[K]
}
