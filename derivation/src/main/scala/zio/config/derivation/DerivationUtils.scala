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

  /**
   * Peel the Describe(Default(Optional( layers and remove the Describe(Optional
   * created by 'implicitOptionDesc'
   *
   * This is used to move the Optional out of the nesting, so a missing record is accepted as None
   */
  def unwrapFromOptional[A](
    configDesc: ConfigDescriptor[A]
  ): (ConfigDescriptor[Any], Boolean) = {
    import zio.config.ConfigDescriptorAdt._
    configDesc match {
      case Default(config, default) =>
        val (inner, opt) = unwrapFromOptional(config.value)
        (Default(lazyDesc(inner), default), opt)
      case Describe(config, message) =>
        config.value match {
          case Optional(config) =>
            (config.value, true)
          case _ =>
            val (inner, opt) = unwrapFromOptional(config.value)
            (Describe(lazyDesc(inner), message), opt)
        }
      case _ =>
        (configDesc.asInstanceOf[ConfigDescriptor[Any]], false)
    }
  }
}
