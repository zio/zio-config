package zio.config

final case class ConfigError(key: Seq[String], error: ConfigError.ErrorType)

object ConfigError {
  sealed trait ErrorType

  case object MissingValue                                    extends ErrorType
  case class ParseError(provided: String, `type`: String)     extends ErrorType
  case class InvalidValue(provided: String, expected: String) extends ErrorType
  case class Unknown(underlying: Throwable)                   extends ErrorType
}
