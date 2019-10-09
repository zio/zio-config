package zio.config

sealed trait ReadError[+K, +V] {
  val key: K
}

object ReadError {
  final case class MissingValue[K](key: K)                               extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, `type`: String) extends ReadError[K, V]
  final case class FoundNestedObject[K](key: K)                          extends ReadError[K, Nothing]
  final case class FatalError[K](key: K, cause: Throwable)               extends ReadError[K, Nothing]
}
