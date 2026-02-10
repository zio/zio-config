package zio.config

import scala.annotation.tailrec

private[config] trait KeyConversionFunctions {

  /**
   * Convert camelCase to any delimited string. Example:
   *
   * {{{
   *   camelToDelimiter("abcDef", "-") === abc-def
   * }}}
   */
  def camelToDelimiter(input: String, delimiter: String): String = {
    def addToAcc(acc: List[String], current: List[Int]) = {
      def currentWord = current.reverse.flatMap(i => Character.toChars(i)).mkString.toLowerCase
      if (current.isEmpty) acc
      else if (acc.isEmpty) currentWord :: Nil
      else currentWord :: delimiter :: acc
    }

    @tailrec
    def loop(chars: List[Int], acc: List[String], current: List[Int], beginning: Boolean): String =
      chars match {
        case Nil                                         => addToAcc(acc, current).reverse.mkString
        case head :: tail if beginning                   =>
          loop(tail, acc, head :: current, Character.isUpperCase(head) || !Character.isLetter(head))
        case head :: tail if Character.isUpperCase(head) =>
          loop(tail, addToAcc(acc, current), head :: Nil, beginning = true)
        case head :: tail                                =>
          loop(tail, acc, head :: current, beginning = false)
      }

    loop(input.map(_.toInt).toList, Nil, Nil, beginning = true)
  }

  /**
   * Convert a camelCase key to kebab-case val s = abcDef toKebabCase(s) === abc-def
   *
   * This version handles numeric boundaries properly:
   *   - "myValue123" -> "my-value-123"
   *   - "QAndA" -> "q-and-a"
   */
  val toKebabCase: String => String =
    camelToDelimiter(_, "-")

  /**
   * Legacy kebab-case conversion that only handles lowercase-uppercase boundaries. Use this for backward compatibility
   * with configs created before zio-config 4.0.5.
   *
   * Example:
   *   - "myValue123" -> "my-value123" (numbers not separated)
   *   - "QAndA" -> "qand-a" (consecutive capitals not separated)
   */
  val toKebabCaseLegacy: String => String =
    input => input.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase

  /**
   * Convert a camelCase key to snake_case
   */
  val toSnakeCase: String => String =
    camelToDelimiter(_, "_")

  /**
   * Add a prefix to an existing key
   */
  def addPrefixToKey(prefix: String): String => String =
    s => s"${prefix}${s.capitalize}"

  /**
   * Add a post fix to an existing key
   */
  def addPostFixToKey(string: String): String => String =
    s => s"${s}${string.capitalize}"
}
