package zio.config

import scala.annotation.tailrec

private[config] trait KeyConversionFunctions {

  /**
   * Splits a camelCase string into words, lower-cases each word and joins the words with `delimiter`. A new word starts
   * at an uppercase letter, unless the current word so far holds only uppercase letters and non-letters. So
   * `maxPoolSize` splits into `max`, `pool` and `size`, `version2Name` splits before `N`, and `HTTPServer` stays one
   * word. Other characters, such as an existing `-` or `_`, are kept.
   *
   * {{{
   * import zio.config._
   *
   * val key: String = camelToDelimiter("maxPoolSize", ".")
   * // max.pool.size
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
   * Converts a camelCase key to kebab-case with `camelToDelimiter(_, "-")`. `maxPoolSize` becomes `max-pool-size` and
   * `version2Name` becomes `version2-name`. Digits stay in the word before them, and a run of capitals at the start of
   * a word stays in that word: `myValue123` becomes `my-value123`, and `QAndA` becomes `qand-a`.
   *
   * {{{
   * import zio.config._
   *
   * val key: String = toKebabCase("version2Name")
   * // version2-name
   * }}}
   */
  val toKebabCase: String => String =
    camelToDelimiter(_, "-")

  /**
   * Converts a camelCase key to kebab-case with the older rule: it inserts `-` only where a lowercase letter `a` to `z`
   * is followed by an uppercase letter `A` to `Z`, then lower-cases the whole key. `toKebabCase` splits there too, and
   * in more places: before an uppercase letter that follows a digit, `_` or `-` in a word that already has a lowercase
   * letter, and between a lowercase and an uppercase letter when either is not ASCII. So `version2Name` becomes
   * `version2name` here and `version2-name` there, `max_Pool` becomes `max_pool` here and `max_-pool` there, and
   * `fooÉtat` becomes `fooétat` here and `foo-état` there. Both turn `myValue123` into `my-value123` and `QAndA` into
   * `qand-a`. Use it to keep keys written for the older rule.
   */
  val toKebabCaseLegacy: String => String =
    input => input.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase

  /**
   * Converts a camelCase key to snake_case with `camelToDelimiter(_, "_")`, splitting words as `toKebabCase` does:
   * `maxPoolSize` becomes `max_pool_size`.
   */
  val toSnakeCase: String => String =
    camelToDelimiter(_, "_")

  /**
   * Returns a function that puts `prefix` before a key and capitalizes the key's first letter, keeping camelCase:
   * `addPrefixToKey("db")("poolSize")` is `dbPoolSize`.
   */
  def addPrefixToKey(prefix: String): String => String =
    s => s"${prefix}${s.capitalize}"

  /**
   * Returns a function that capitalizes the first letter of `string` and puts it after a key, keeping camelCase:
   * `addPostFixToKey("timeout")("read")` is `readTimeout`.
   */
  def addPostFixToKey(string: String): String => String =
    s => s"${s}${string.capitalize}"
}
