package zio.config

import zio.config.ReadErrors.ReadError

final case class ReadErrors[K, V](errors: ::[ReadError[K, V]])

object ReadErrors {
  def apply[K, V](a: ReadError[K, V], as: ReadError[K, V]*): ReadErrors[K, V] =
    ReadErrors(::(a, as.toList))

  def concat[K, V](l: ReadErrors[K, V], r: ReadErrors[K, V]): ReadErrors[K, V] =
    ReadErrors(::(l.errors.head, l.errors.tail ++ r.errors))

  sealed trait ReadError[+K, +V] {
    val key: K
  }

  object ReadError {
    final case class MissingValue[K, V](key: K)                             extends ReadError[K, V]
    final case class ParseError[K, V](key: K, provided: V, message: String) extends ReadError[K, V]
    final case class FatalError[K, V](key: K, cause: Throwable)             extends ReadError[K, V]
  }
}
