package zio.config

import scala.annotation.tailrec
import scala.collection.JavaConverters._

private[config] trait KeyConversionFunctions {
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
        case Nil => addToAcc(acc, current).reverse.mkString
        case head :: tail if beginning =>
          loop(tail, acc, head :: current, Character.isUpperCase(head) || !Character.isLetter(head))
        case head :: tail if Character.isUpperCase(head) =>
          loop(tail, addToAcc(acc, current), head :: Nil, beginning = true)
        case head :: tail =>
          loop(tail, acc, head :: current, beginning = false)
      }

    loop(input.codePoints().iterator().asScala.map(x => x: Int).toList, Nil, Nil, beginning = true)
  }

  val toKebabCase: String => String =
    camelToDelimiter(_, "-")

  val toSnakeCase: String => String =
    camelToDelimiter(_, "_")

  def addPrefix(prefix: String): String => String =
    s => s"${prefix}${s}"

  def postFix(string: String): String => String =
    s => s"${s}${string}"
}
