package zio.config

sealed trait ReadError[+K, +V]

object ReadError {
  final case class MissingValue[K](key: K, positions: Option[Int] = None)                    extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, message: String)                    extends ReadError[K, V]
  final case class Unknown[K](key: K, cause: Throwable)                                      extends ReadError[K, Nothing]
  final case class OrErrors[K, V](leftErrors: ReadError[K, V], rightErrors: ReadError[K, V]) extends ReadError[K, V]
  final case class AndErrors[K, V](errors: ::[ReadError[K, V]])                              extends ReadError[K, V]
}
