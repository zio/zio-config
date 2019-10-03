package zio.config

sealed trait ReadError {
  val key: String
}

object ReadError {
  case class MissingValue(key: String)                                 extends ReadError
  case class ParseError(key: String, provided: String, `type`: String) extends ReadError
}
