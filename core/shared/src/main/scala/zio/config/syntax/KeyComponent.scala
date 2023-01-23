package zio.config.syntax

sealed trait KeyComponent

object KeyComponent {
  final case class Index(index: Int)      extends KeyComponent
  final case class KeyName(value: String) extends KeyComponent
}
