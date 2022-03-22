package zio.config

package object magnolia {
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

  def descriptorForPureConfig[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorForPureConfig[A]

  def descriptorWithClassNames[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorWithClassNames[A]

  def descriptorWithoutClassNames[A](implicit config: Descriptor[A]): ConfigDescriptor[A] =
    Descriptor.descriptorWithoutClassNames[A]

  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe
  type name = derivation.name
  val name: derivation.name.type = derivation.name
  type names = derivation.names
  val names: derivation.names.type = derivation.names
}
