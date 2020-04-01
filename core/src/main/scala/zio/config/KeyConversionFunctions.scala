package zio.config

import scala.annotation.tailrec

private[config] trait KeyConversionFunctions {
  private def camelToDelimiter(delimiter: String): String => String = s => {
    @tailrec def loop(s: String, output: String, lastUppercase: Boolean): String =
      if (s.isEmpty) output
      else {
        val c = if (s.head.isUpper && !lastUppercase) delimiter + s.head.toLower else s.head.toLower
        loop(s.tail, output + c, s.head.isUpper && !lastUppercase)
      }

    loop(s, "", lastUppercase = true)
  }

  val camelToKebab: String => String =
    camelToDelimiter("-")

  val camelToSnake: String => String =
    camelToDelimiter("_")

  def addPrefix(prefix: String): String => String =
    s => s"${prefix}${s}"

  def postFix(string: String): String => String =
    s => s"${s}${string}"
}
