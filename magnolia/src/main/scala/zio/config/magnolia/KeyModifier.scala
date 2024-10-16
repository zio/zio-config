package zio.config
package magnolia

sealed trait KeyModifier
sealed trait CaseModifier extends KeyModifier

object KeyModifier {
  case object KebabCase               extends CaseModifier
  case object SnakeCase               extends CaseModifier
  case object NoneModifier            extends CaseModifier
  case class Prefix(prefix: String)   extends KeyModifier
  case class Postfix(postfix: String) extends KeyModifier
  case class Suffix(suffix: String)   extends KeyModifier

  def getModifierFunction(keyModifier: KeyModifier): String => String =
    keyModifier match {
      case KebabCase        => toKebabCase
      case SnakeCase        => toSnakeCase
      case Prefix(prefix)   => addPrefixToKey(prefix)
      case Postfix(postfix) => addPostFixToKey(postfix)
      case Suffix(suffix)   => addSuffixToKey(suffix)
      case NoneModifier     => identity
    }
}
