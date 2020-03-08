package zio.config

sealed trait ReadError

object ReadError {
  final case class MissingValue(key: String, positions: Option[Int] = None) extends ReadError
  final case class ParseError(key: String, message: String)                 extends ReadError
  final case class Unknown(key: String, cause: Throwable)                   extends ReadError
  final case class OrErrors(leftErrors: ReadError, rightErrors: ReadError)  extends ReadError
  final case class AndErrors(leftErrors: ReadError, rightErrors: ReadError) extends ReadError
}
