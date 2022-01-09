package zio.config.yaml

import org.snakeyaml.engine.v2.env.EnvConfig

import java.util.Optional

/**
 * Configurator for ENV format
 */
object EnvConfigImpl extends EnvConfig {

  /**
   * Implement deviation from the standard logic.
   *
   * @param name        - variable name in the template
   * @param separator   - separator in the template, can be :-, -, :?, ? or null if not present
   * @param value       - default value or the error in the template or empty if not present
   * @param environment - the value from environment for the provided variable or null if unset
   * @return the value to apply in the template or empty to follow the standard logic
   */
  override def getValueFor(name: String, separator: String, value: String, environment: String): Optional[String] =
    (Option(environment), Option(separator)) match {
      case (None, Some(sep)) if sep == ":?" || sep == "?" => throw new Exception(value)
      case (None, Some(sep)) if sep == ":-" || sep == "-" => Optional.ofNullable(value)
      case (None, _)                                      => Optional.of(s"$${$name}")
      case (Some(v), _)                                   => Optional.of(v)
    }
}
