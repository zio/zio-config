package zio.config.magnolia

import zio.config._

/**
 * A copy of zio-config-derivation util, to be able to map a constant value
 * to case class that has the same name as that of the constant value, with zero
 * number of fields in it.
 */
final case class Constant(value: String) extends PropertyType[String, String] {
  def read(propertyValue: String): Either[PropertyType.PropertyReadError[String], String] =
    if (propertyValue == value) Right(value)
    else Left(PropertyType.PropertyReadError(propertyValue, s"constant string '$value'"))

  def write(a: String): String = a
}

object Constant {
  def mk(value: String): ConfigDescriptor[String] =
    ConfigDescriptorAdt.Source(ConfigSource.empty, Constant(value)) ?? s"constant string '$value'"
}