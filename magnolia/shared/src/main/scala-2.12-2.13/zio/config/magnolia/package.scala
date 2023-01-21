package zio.config

import zio.Config

package object magnolia {

  def getDescriptor_[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

  def getDescriptor[A](desc: ConfigDescriptor[A]): Descriptor[A] =
    Descriptor[A](desc)

  /**
   * descriptor[A] allows the user to automatically derive `ConfigDescriptor` instead
   * of using the ConfigDescriptor dsl explicitly (i.e, manual implementation).
   * While manual implementation can be verbose, it is a recommended to use it when it comes to simple configurations.
   *
   * On the other hand, automatic derivation can become handly when the config is complex with relatively larger number of parameters,
   * or when it is constantly changing during the software lifecycle, or it's just complex structure with nested products and coproducts.
   *
   * Below given is a small example to show the usage of `descriptor[A]`.
   *
   * Example :
   *
   * {{{
   *     final case class MyConfig(appName: String, port: Int, jdbcUrl: String)
   *
   *     val configDesc: ConfigDescriptor[MyConfig]
   *
   *     val config = read(configDesc from ConfigSource.fromMap(Map.empty))
   *
   * }}}
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   *
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   *
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   *  In this case, define a Descriptor for ZonedDateTime using
   *
   *  {{{
   *
   *    implicit def deriveForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   *
   *    descriptor[MyConfig] // then works
   *  }}}
   */
  def descriptor[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptor[A]

  /**
   * Derive ConfigDescriptor that can read ADTs, such that the type of the sub-class
   * should be encoded as an extra key using the label `type`. Example:
   *     {{{
   *       sealed trait X
   *       case class A(name: String) extends X
   *       case class B(age: Int) extends X
   *
   *       case class AppConfig(x: X)
   *
   *       val str =
   *         s"""
   *          x : {
   *            type = A
   *            name = jon
   *          }
   *         """
   *
   *       read(descriptorForPureConfig[AppConfig] from ConfigSource.fromHoconString(str))
   *     }}}
   */
  def descriptorForPureConfig[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorForPureConfig[A]

  /**
   * Derive ConfigDescriptor that consider the names sealed-traits and names of case-classes
   * that are subtypes of a sealed-trait.
   *
   * {{{
   *   sealed trait Y
   *
   *  object Y {
   *    case class A(age: Int)     extends Y
   *    case class B(name: String) extends Y
   *  }
   *
   *  case class AppConfig(x: Y)
   *
   *  val str =
   *    s"""
   *             x : {
   *                   Y : {
   *                      A : {
   *                        age : 10
   *                      }
   *                 }
   *             }
   *            """
   *
   *  zio.Runtime.default.unsafeRun(
   *    read(descriptorWithClassNames[AppConfig] from ConfigSource.fromHoconString(str))
   *  )
   * }}}
   */
  def descriptorWithClassNames[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorWithClassNames[A]

  /**
   * Derive ConfigDescriptor that consider the names sealed-traits and names of case-classes
   * that are subtypes of a sealed-trait, such that the name is encoded in a `label`, similar
   * to that of pure-config.
   *
   *     {{{
   *  sealed trait X
   *
   *  object X {
   *    case class A(name: String) extends X
   *    case class B(age: Int)     extends X
   *  }
   *
   *  sealed trait Y
   *
   *  object Y {
   *    case class A(age: Int)     extends Y
   *    case class B(name: String) extends Y
   *  }
   *
   *  case class AppConfig(x: Either[X, Y])
   *
   *  val str =
   *    s"""
   *             x : {
   *                   Y : {
   *                    type = A
   *                    age = 10
   *               }
   *             }
   *            """
   *
   *  zio.Runtime.default.unsafeRun(
   *    read(descriptorWithClassNames[AppConfig]("type") from ConfigSource.fromHoconString(str))
   *  )
   *     }}}
   */
  def descriptorWithClassNames[A](label: String)(implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorWithClassNames[A](label)

  /**
   * Derive ConfigDescriptor discarding the names of sealed-trait and case-classes
   *
   * {{{
   *   sealed trait Y
   *
   *  object Y {
   *    case class A(age: Int)     extends Y
   *    case class B(name: String) extends Y
   *  }
   *
   *  case class AppConfig(x: Y)
   *
   *  val str =
   *    s"""
   *             x : {
   *               age : 10
   *             }
   *            """
   *
   *  zio.Runtime.default.unsafeRun(
   *    read(descriptorWithoutClassNames[AppConfig] from ConfigSource.fromHoconString(str))
   *  )
   *
   * }}}
   */
  def descriptorWithoutClassNames[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorWithoutClassNames[A]

  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe
  type name = derivation.name
  val name: derivation.name.type = derivation.name
  type names = derivation.names
  val names: derivation.names.type = derivation.names
}
