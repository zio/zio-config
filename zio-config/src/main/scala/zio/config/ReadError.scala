package zio.config

final case class ReadError(key: String, error: ReadError.ErrorType)

object ReadError {
  sealed trait ErrorType

  case object MissingValue                                extends ErrorType
  case class ParseError(provided: String, `type`: String) extends ErrorType
}
