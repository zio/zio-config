package zio.config.magnolia

import zio.config._

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
