package zio.config.derivation

import zio.config.ConfigDescriptorAdt._
import zio.config._

import scala.annotation.tailrec

object DerivationUtils {
  case class ConstantString(value: String) extends PropertyType[String, String] {
    def read(propertyValue: String): Either[PropertyType.PropertyReadError[String], String] =
      if (propertyValue == value) Right(value)
      else Left(PropertyType.PropertyReadError(propertyValue, s"constant string '$value'"))
    def write(a: String): String                                                            = a
  }

  def constantString(value: String): ConfigDescriptor[String] =
    ConfigDescriptorAdt.Source(ConfigSource.empty, ConstantString(value)) ?? s"constant string '$value'"

  def constant[T](label: String, value: T): ConfigDescriptor[T] =
    constantString(label)(_ => value, p => Some(p).filter(_ == value).map(_ => label))

  /**
   * FIXME: Investigate why this logic, and see if we can avoid
   * Peel the Describe(Default(Optional( layers and remove the Describe(Optional
   * created by 'implicitOptionDesc'
   *
   * This is used to move the Optional out of the nesting, so a missing record is accepted as None
   */
  private[config] def unwrapFromOptional[A](
    configDesc: ConfigDescriptor[A]
  ): (ConfigDescriptor[Any], Boolean) =
    configDesc match {
      case Lazy(thunk)               => unwrapFromOptional(thunk())
      case Default(config, default)  =>
        val (inner, opt) = unwrapFromOptional(config)
        (Default(lazyDesc(inner), default), opt)
      case Describe(config, message) =>
        unwrapThunk(config, message)
      case _                         =>
        (configDesc.asInstanceOf[ConfigDescriptor[Any]], false)
    }

  // Kept to make tests work with unwrapFromOptional logic.
  @tailrec
  private[config] def unwrapThunk[A](
    config: ConfigDescriptor[A],
    message: String
  ): (ConfigDescriptor[Any], Boolean) =
    config match {
      case Lazy(thunk)                             => unwrapThunk(thunk(), message)
      case Optional(config: ConfigDescriptor[Any]) =>
        (config, true)
      case _                                       =>
        val (inner, opt) = unwrapFromOptional(config)
        (Describe(lazyDesc(inner), message), opt)

    }
}
