package zio.config.derivation

import zio.config._

object DerivationUtils {
  case class ConstantString(value: String) extends PropertyType[String, String] {
    def read(propertyValue: String): Either[PropertyType.PropertyReadError[String], String] =
      if (propertyValue == value) Right(value)
      else Left(PropertyType.PropertyReadError(propertyValue, s"constant string '$value'"))
    def write(a: String): String = a
  }

  def constantString(value: String): ConfigDescriptor[String] =
    ConfigDescriptorAdt.Source(ConfigSource.empty, ConstantString(value)) ?? s"constant string '$value'"

  def constant[T](label: String, value: T): ConfigDescriptor[T] =
    constantString(label)(_ => value, p => Some(p).filter(_ == value).map(_ => label))
}
