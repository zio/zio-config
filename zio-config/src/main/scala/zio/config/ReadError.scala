package zio.config

sealed trait ReadError[+K, +V] {
  def key: K
}

object ReadError {
  final case class MissingValue[K](key: K)                                extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, message: String) extends ReadError[K, V]
  final case class FatalError[K](key: K, cause: Throwable)                extends ReadError[K, Nothing]
}
