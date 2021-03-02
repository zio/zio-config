package zio.config.magnolia

import zio.config._

case class Descriptor[T](desc: ConfigDescriptor[T], isObject: Boolean = false) {

  /**
   * To add documentation while defining an instance of Descriptor.
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   * }}}
   *
   * With `??`, you can provide documentation as well.
   *
   * That is:
   *
   * {{{
   *     implicit def deriveForZonedDateTime: Descriptor[ZonedDateTime] =
   *       descriptorForZonedDateTime ?? "Time in UTC"
   *
   *     descriptor[MyConfig] // then works
   *  }}}
   */
  final def ??(description: String): Descriptor[T] =
    describe(description)

  /**
   * To provide default values while defining an instance of Descriptor.
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   * }}}
   *
   * With `default`, you can provide documentation as well.
   *
   * That is:
   *
   * {{{
   *     implicit def deriveForZonedDateTime: Descriptor[ZonedDateTime] =
   *       descriptorForZonedDateTime.default(ZonedDateTime.now())
   *
   *     descriptor[MyConfig] // then works
   *  }}}
   */
  def default(value: T): Descriptor[T] =
    Descriptor(desc.default(value))

  /**
   * To add documentation while defining an instance of Descriptor.
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   * }}}
   *
   * With `describe`, you can provide documentation as well.
   *
   * That is:
   *
   * {{{
   *     implicit def deriveForZonedDateTime: Descriptor[ZonedDateTime] =
   *       descriptorForZonedDateTime describe "Time in UTC"
   *
   *     descriptor[MyConfig] // then works
   *  }}}
   */
  def describe(description: String): Descriptor[T] =
    Descriptor(desc.describe(description))

  /**
   * To provide source while defining an instance for descriptor
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime, username: String)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   * }}}
   *
   * With `from`, you can provide documentation as well.
   *
   * That is:
   *
   * {{{
   *     val defaultSource: ConfigSource = ???
   *
   *     implicit def deriveForZonedDateTime: Descriptor[ZonedDateTime] =
   *       descriptorForZonedDateTime from defaultSource
   *
   *     val envSource: ConfigSource = ???
   *
   *     val result: Either[ReadError[String], MyConfig] = read(descriptor[MyConfig] from envSource)
   *  }}}
   *
   *  For ZonedDateTime, it always tries the `defaultSource` first, and then if it fails tries the `envSource`.
   */
  def from(that: ConfigSource): Descriptor[T] =
    Descriptor(desc.from(that))

  /**
   * `transform` allows us to define instance of `Descriptor`
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    implicit val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transform(string => ZonedDateTime.parse(string), _.toString)
   *
   *     descriptor[MyConfig] // now works
   * }}}
   *
   * However, we recommend you using `transformOrFail`, because in the above case `ZonedDateTime.parse(string)` may fail and isn't handled anywhere.
   * With transformOrFail, its properly handled and will be part of the error message if zio-config fails to retrieve the config.
   */
  def transform[B](f: T => B, g: B => T): Descriptor[B] =
    Descriptor(desc.transform(f, g))

  /**
   * `transformOrFail` allows us to define instance of `Descriptor`
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    implicit val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFail(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, r => Right(r.toString))
   *
   *    descriptor[MyConfig] // now works
   * }}}
   *
   * You can also see the `ZonedDateTime => String` doesn't fail, and we had to lift to Either type using `Right` constructor.
   * Hence it is better off using `transformOrFailLeft` in this case.
   */
  def transformOrFail[B](f: T => Either[String, B], g: B => Either[String, T]): Descriptor[B] =
    Descriptor(desc.transformOrFail(f, g))

  /**
   * `transformOrFailLeft` allows us to define instance of `Descriptor`
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    implicit val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFailLeft(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, _.toString)
   *
   *    descriptor[MyConfig] // now works
   * }}}
   */
  def transformOrFailLeft[B](f: T => Either[String, B])(g: B => T): Descriptor[B] =
    Descriptor(desc.transformOrFailLeft(f)(g))

  /**
   * `transformOrFailRight` allows us to define instance of `Descriptor`
   *
   * To give an overview of what `Descriptor`:
   *
   * `descriptor[MyConfig]` works only if all the types that forms `MyConfig` has an instance of `Descriptor`.
   * For almost all the important types, zio-config-magnolia already provides implicit instances for `Descriptor`.
   *
   * However, say you have a type ZonedDateTime, for which zio-config hasn't provided instance of `Descriptor`, then it will fail to compile.
   * {{{
   *      case class MyConfig(x: ZonedDateTime)
   * }}}
   *
   * In this case, define a Descriptor for ZonedDateTime using
   *
   * {{{
   *    implicit val descriptorForZonedDateTime: Descriptor[ZonedDateTime] =
   *     Descriptor[String].transformOrFailRight(string => Try(ZonedDateTime.parse(string).toEither.swap.map(_.getMessage).swap, _.toString)
   *
   *    descriptor[MyConfig] // now works
   * }}}
   */
  def transformOrFailRight[E, B](f: T => B, g: B => Either[String, T]): Descriptor[B] =
    Descriptor(desc.transformOrFailRight(f, g))
}

object Descriptor extends DeriveConfigDescriptor {
  def apply[A](implicit ev: Descriptor[A]): Descriptor[A] = ev

  sealed trait SealedTraitSubClassNameStrategy {
    def &&(
      sealedTraitNameStrategy: SealedTraitNameStrategy
    ): SealedTraitStrategy =
      SealedTraitStrategy(this, sealedTraitNameStrategy)
  }

  object SealedTraitSubClassNameStrategy {
    case object WrapSubClassName                    extends SealedTraitSubClassNameStrategy
    case object IgnoreSubClassName                  extends SealedTraitSubClassNameStrategy
    case class LabelSubClassName(fieldName: String) extends SealedTraitSubClassNameStrategy
  }

  sealed trait SealedTraitNameStrategy {
    def &&(
      subClassNameStrategy: SealedTraitSubClassNameStrategy
    ): SealedTraitStrategy =
      SealedTraitStrategy(subClassNameStrategy, this)
  }

  object SealedTraitNameStrategy {
    case object WrapSealedTraitName   extends SealedTraitNameStrategy
    case object IgnoreSealedTraitName extends SealedTraitNameStrategy
  }

  case class SealedTraitStrategy(
    subClass: SealedTraitSubClassNameStrategy,
    parentClass: SealedTraitNameStrategy
  )

  object SealedTraitStrategy {
    import SealedTraitNameStrategy._
    import SealedTraitSubClassNameStrategy._

    def wrapSealedTraitName: SealedTraitNameStrategy   = WrapSealedTraitName
    def ignoreSealedTraitName: SealedTraitNameStrategy = IgnoreSealedTraitName

    def wrapSubClassName: SealedTraitSubClassNameStrategy                     = WrapSubClassName
    def ignoreSubClassName: SealedTraitSubClassNameStrategy                   = IgnoreSubClassName
    def labelSubClassName(fieldName: String): SealedTraitSubClassNameStrategy = LabelSubClassName(fieldName)
  }
}
