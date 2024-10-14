package zio.config.yaml.generators

/**
 * A printer that ensures only strings are concatenated
 */
object Printer {
  def print(strings: String*): String = strings.toList.mkString
}
