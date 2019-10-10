package zio.config

sealed trait ReadError extends Exception {
  val key: String
}

final case class ReadErrors(errors: ::[ReadError]) extends Exception {
  override def getMessage(): String = ???
}

object ReadErrors {
  def apply(a: ReadError, as: ReadError*): ReadErrors =
    ReadErrors(::(a, as.toList))

  def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
    ReadErrors(::(l.errors.head, l.errors.tail ++ r.errors))
}

object ReadError {
  final case class MissingValue(key: String)                                 extends ReadError
  final case class ParseError(key: String, provided: String, `type`: String) extends ReadError
  final case class FatalError(key: String, cause: Throwable)                 extends ReadError
}
