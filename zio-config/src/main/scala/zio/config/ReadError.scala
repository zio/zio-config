package zio.config

sealed trait ReadError {
  val key: String
}

object ReadError {
  final case class MissingValue(key: String, cause: String)                  extends ReadError
  final case class ParseError(key: String, provided: String, `type`: String) extends ReadError
  final case class FatalError(key: String, cause: Throwable)                 extends ReadError
}
