package zio.config

import scala.util.control.NoStackTrace

sealed trait ReadError[+K, +V] extends NoStackTrace {
  val key: K
}

final case class ReadErrors[K, V](errors: ::[ReadError[K, V]]) extends NoStackTrace

object ReadErrors {
  def apply[K, V](a: ReadError[K, V], as: ReadError[K, V]*): ReadErrors[K, V] =
    ReadErrors(::(a, as.toList))

  def concat[K, V](l: ReadErrors[K, V], r: ReadErrors[K, V]): ReadErrors[K, V] =
    ReadErrors(::(l.errors.head, l.errors.tail ++ r.errors))
}

object ReadError {
  final case class MissingValue[K](key: K)                                extends ReadError[K, Nothing]
  final case class ParseError[K, V](key: K, provided: V, message: String) extends ReadError[K, V]
  final case class FatalError[K](key: K, cause: Throwable)                extends ReadError[K, Nothing]
}
