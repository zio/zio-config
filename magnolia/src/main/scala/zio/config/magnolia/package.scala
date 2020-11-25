package zio.config

package object magnolia {
  type Descriptor[A] = DeriveConfigDescriptor.Descriptor[A]

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
   *
   * `descriptor[A]` can also handle sealed traits, maps list etc.
   *
   * Example:
   *
   * {{{
   *    sealed trait A
   *
   *    object A {
   *      case class B(x: String, y: String) extends A
   *      case class C(z: String) extends A
   *      case object D extends A
   *    }
   *
   *    val config = descriptor[A]
   *
   *    val mapSource = ConfigSource.fromMap(Map("B.x" -> "l", "B.y" -> "m")
   *    val result = read(config from mapSource)
   *    // Right(B("x", "y"))
   *
   *    val typesafeSource = TypesafeConfigSource.fromHoconString(
   *      s"""
   *        {
   *          B : {
   *             x : l
   *             y : m
   *          }
   *        }
   *
   *      """
   *
   *      val result = typesafeSource.flatMap(source => read(config from source))
   *
   *      // Right(B("x", "y"))
   *    )
   * }}}
   *
   * While sealed trait can be fairly straight forward, there are historical errors users make
   * with any advanced config libraries.
   *
   * Example: What happens if there is another `B` in the same package but for a different parent sealed trait name ?
   *
   *  {{{
   *    sealed trait X
   *
   *    object X {
   *      case class B(x: String, y: String) extends X
   *      case class C(z: String) extends X
   *      case object D extends X
   *    }
   *
   *    sealed trait Y
   *
   *    object Y {
   *      case class B(x: String, y: String) extends Y
   *      case class Z(value: String) extends Y
   *    }
   *
   *    final case class MyConfig(xOrY: Either[X, Y])
   *
   *    val typesafeSource =
   *      TypesafeConfigSource.fromHoconString(
   *        s"""
   *         xOrY: {
   *           B : {
   *              x : l,
   *              y : m
   *           }
   *         }
   *       """
   *      )
   *
   *   }}}
   *
   *   For zio-config, Either[X, Y] implies, it tries to fetch X and if it fails, it falls over to trying
   *   to read Y.
   *
   *   However, in the above case, the output will be always X while user might have intended to provide Y.
   *
   *   This was just an example, but similar conflicts can occur and zio-config-magnolia has strong semantics to handle such scenarios.
   *   The best way is to indicate the name of the sealed trait itself.
   *
   *   That is
   *
   *   Example:
   *
   *   {{{
   *      import zio.config._
   *
   *      // This implies, not only we are making use of the names of the case classes (or case objects) but the actual
   *      // name of the sealed trait as well.
   *      val betterDerivation = new DeriveConfigDescriptor {
   *         override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
   *           wrapSubClassName && wrapSealedTraitName
   *     }
   *   }}}
   *
   *
   *   If the source is HOCON, then {{{ betterDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *      xOrY: {
   *        X : {
   *           B : {
   *              x : xyz
   *              y : xyz
   *           }
   *         }
   *      }
   *   }}}
   *
   *
   * Providing the name of the sealed traits is least commonly used. This is why the default derivation of sealed trait doesn't consider it.
   *
   * There is a third way of config derivation, especially for those who would like to migrate pure-config's implementation.
   * In this case, we ignore the sealed-trait name, but we consider the sub-class name but not as a parent but part of the product itself.
   *
   * {{{
   *    import zio.config._
   *    val customDerivation = new DeriveConfigDescriptor {
   *      override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
   *        labelSubClassName("type") && ignoreSealedTraitName
   *   }
   * }}}
   *
   * If the source is HOCON, then {{{ betterDerivation.descriptor[MyConfig] }}} can read:
   *
   *
   *  {{{
   *     x: {
   *       type : B
   *       x : r
   *       y : z
   *    }
   *  }}}
   *
   */
  def descriptor[A](implicit config: Descriptor[A]): ConfigDescriptor[A] = DeriveConfigDescriptor.descriptor[A]

  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe
  type name = derivation.name
  val name: derivation.name.type = derivation.name
}
