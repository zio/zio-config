package zio.config

sealed trait ReadError[+K, +V] {
  def key: K
}

object ReadError {
  final case class MissingValue[K](key: K)                                extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, message: String) extends ReadError[K, V]
  final case class FatalError[K](key: K, cause: Throwable)                extends ReadError[K, Nothing]

  def missingValue[K](key: K): ReadError[K, Nothing] =
    MissingValue(key)

  def parseError[K, V](key: K, provided: V, message: String): ReadError[K, V] =
    ParseError(key, provided, message)

  def fatalError[K, V](key: K, cause: Throwable): ReadError[K, V] =
    FatalError(key, cause)
}
