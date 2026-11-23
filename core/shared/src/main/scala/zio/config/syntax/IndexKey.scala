package zio.config.syntax

/**
 * Builds the bracketed index that marks a list element in a flat key, as in `ports[0]`.
 */
object IndexKey {

  /**
   * Returns `value` in square brackets: `IndexKey(3)` is `"[3]"`. The value isn't checked, so `IndexKey(-1)` is
   * `"[-1]"`.
   */
  def apply(value: Int): String = s"[$value]"
}
