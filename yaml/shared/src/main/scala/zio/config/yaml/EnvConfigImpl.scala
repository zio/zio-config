package zio.config.yaml

import org.snakeyaml.engine.v2.env.EnvConfig

import java.util.Optional

/**
 * The snakeyaml-engine `EnvConfig` that [[YamlConfigProvider]] uses to replace a value written as `\${NAME}` with the
 * environment variable `NAME`. The expression must be the whole value and unquoted, and a default or message after the
 * separator may contain only letters, digits and `_`; any other value, such as `\${NAME:-a/b}`, `"\${NAME}"` or
 * `http://\${NAME}`, is kept as written. A variable that is set, even to the empty string, is used as is. When the
 * variable is not set, `\${NAME:-default}` and `\${NAME-default}` give `default`, `\${NAME:?message}` and
 * `\${NAME?message}` make parsing throw a `java.lang.Exception` whose message is `message`, and `\${NAME}` keeps the
 * text `\${NAME}`.
 */
object EnvConfigImpl extends EnvConfig {

  /**
   * Returns the text that replaces one `\${...}` expression. The YAML parser calls it for each unquoted value that is
   * exactly `\${NAME}`, `\${NAME-default}`, `\${NAME:-default}`, `\${NAME?message}` or `\${NAME:?message}`, where the
   * default or message contains only letters, digits and `_`. When the variable is not set and the separator is `:?` or
   * `?`, it throws a `java.lang.Exception` with `value` as the message.
   *
   * @param name
   *   the variable name in the expression
   * @param separator
   *   the separator in the expression: `:-`, `-`, `:?`, `?`, or `null` when there is none
   * @param value
   *   the default value or the error message that follows the separator
   * @param environment
   *   the value of the environment variable, or `null` when it is not set
   * @return
   *   `environment` when the variable is set; otherwise `value` for `:-` and `-`, and the original text `\${name}` when
   *   there is no separator
   */
  override def getValueFor(name: String, separator: String, value: String, environment: String): Optional[String] =
    (Option(environment), Option(separator)) match {
      case (None, Some(sep)) if sep == ":?" || sep == "?" => throw new Exception(value)
      case (None, Some(sep)) if sep == ":-" || sep == "-" => Optional.ofNullable(value)
      case (None, _)                                      => Optional.of(s"$${$name}")
      case (Some(v), _)                                   => Optional.of(v)
    }
}
