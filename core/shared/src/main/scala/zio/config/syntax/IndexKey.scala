package zio.config.syntax

object IndexKey {
  def apply(value: Int): String = s"[$value]"
}
