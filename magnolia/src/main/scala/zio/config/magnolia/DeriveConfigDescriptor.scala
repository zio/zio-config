package zio.config.magnolia

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import magnolia._
import zio.config._
import zio.config.derivation.DerivationUtils
import zio.config.derivation.DerivationUtils._
import zio.duration.Duration

import scala.concurrent.duration.{ Duration => ScalaDuration }
import scala.language.experimental.macros

/**
 * DeriveConfigDescriptor.descriptor[Config] gives an automatic ConfigDescriptor for the case class Config recursively
 *
 * DeriveConfigDescriptor.descriptor[X] gives an automatic ConfigDescriptor for the sealed trait X (coproduct)
 *
 * {{{
 *
 *    // Given
 *    final case class Config(username: String, age: Int)
 *
 *    // should work with no additional code
 *    val description = descriptor[Config]
 *
 *    val config = Config.fromSystemEnv(description)
 *
 * }}}
 *
 *
 * Please find more (complex) examples in the examples module in zio-config
 */
object DeriveConfigDescriptor extends DeriveConfigDescriptor {
  def mapClassName(name: String): String = toSnakeCase(name)
  def mapFieldName(name: String): String = name

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = false
}

trait DeriveConfigDescriptor { self =>

  /**
   *
   * Strategy to how to name the class names in the actual config (if they are used in the config)
   *
   * Example:
   *
   *  {{{
   *    sealed trait Credentials
   *
   *    object Credentials {
   *      final case class UsernamePassword(username: String, password: String) extends Credentials
   *      final case class Token(username: String, tokenId: String) extends Credentials.
   *    }
   *
   *    final case class MyConfig(auth: Credentials)
   *  }}}
   *
   *  If the source is HOCON and {{{ def mapClassName(name: String): String = toSnakeCase(name) }}}, then descriptor[MyConfig] can read:
   *
   *   {{{
   *     auth : {
   *       username_password : {
   *          username : xyz
   *          password : abc
   *
   *       }
   *     }
   *   }}}
   *
   *   If snake_case is not your choice and need to change to something else (example: kebabCase),
   *   there are 2 alternative solutions,
   *
   *   {{{
   *    sealed trait Credentials
   *
   *    @name("username-password")
   *    case class UsernamePassword(username: String, password: String) extends Credentials
   *
   *    @name("token")
   *    case class Token(username: String, tokenId: String) extends Credentials.
   *   }}}
   *
   *   With the above structure, if the source is HOCON, descriptor[Credentials] can read:
   *
   *   {{{
   *     auth : {
   *       username-password : {
   *          username : xyz
   *          password : abc
   *       }
   *     }
   *   }}}
   *
   *   If you need the kebabCase to be applied for all the sealed-traits in your entire app,
   *   then the second, and the best solution is to do the following:
   *
   *   {{{
   *     import zio.config._
   *
   *     val customDerivation = new DeriveConfigDescriptor {
   *         def mapClassName(name: String): String = camelToKebab(name)
   *         def mapFieldName(name: String): String = name
   *
   *         val wrapSealedTraitClasses: Boolean = true
   *         val wrapSealedTraits: Boolean       = false
   *     }
   *
   *     // Usage:
   *     customDerivation.descriptor[MyConfig]
   *
   *   }}}
   *
   *   If the source is HOCON, then {{{ customDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *     auth : {
   *       username-password : {
   *          username : xyz
   *          password : abc
   *       }
   *     }
   *   }}}
   */
  def mapClassName(name: String): String

  /**
   *
   *  trategy to how to name the class names in the actual config (if they are used in the config)
   *
   * Now assume that you need the fieldnames and classnames to be UPPERCASE in the config, and not camelCase,
   * then you could do the following
   *
   *   {{{
   *      val customDerivation = new DeriveConfigDescriptor {
   *        def mapClassName(name: String): String = name.toUpperCase
   *        def mapFieldName(name: String): String = name.toUpperCase
   *
   *        val wrapSealedTraitClasses: Boolean = true
   *        val wrapSealedTraits: Boolean       = false
   *      }
   *
   *      // Usage:
   *      customDerivation.descriptor[MyConfig]
   *   }}}
   *
   *  Given,
   *
   *  {{{
   *    sealed trait Credentials
   *
   *    object Credentials {
   *      final case class UsernamePassword(username: String, password: String) extends Credentials
   *      final case class Token(username: String, tokenId: String) extends Credentials.
   *    }
   *
   *    final case class MyConfig(auth: Credentials)
   *  }}}
   *
   *  If the source is HOCON, then {{{ customDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *       AUTH : {
   *         USERNAMEPASSWORD : {
   *            USERNAME : xyz
   *            PASSWORD : abc
   *         }
   *       }
   *   }}}
   */
  def mapFieldName(name: String): String

  /**
   *
   *  Strategy to deal with the use of classnames in the config.
   *
   *  Now say, you need to skip the use of classnames in the config, then the following is the solution:
   *
   *   {{{
   *      import zio.config._
   *
   *      val customDerivation = new DeriveConfigDescriptor {
   *        def mapClassName(name: String): String = camelToKebab(name)
   *        def mapFieldName(name: String): String = camelToKebab(name)
   *
   *        val wrapSealedTraitClasses: Boolean = false
   *        val wrapSealedTraits: Boolean       = false
   *     }
   *   }}}
   *
   *  Given,
   *
   *  {{{
   *    sealed trait Credentials
   *
   *    object Credentials {
   *      final case class UsernamePassword(username: String, password: String) extends Credentials
   *      final case class Token(username: String, tokenId: String) extends Credentials.
   *    }
   *
   *    final case class MyConfig(auth: Credentials)
   *  }}}
   *
   *  If the source is HOCON, then {{{ customDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *     auth : {
   *       username : xyz
   *       password : abc
   *     }
   *   }}}
   *
   *   However, we don't recommend you doing it.
   */
  def wrapSealedTraitClasses: Boolean

  /**
   *
   * Strategy to deal with the use of sealed-trait name in the config.
   *
   * Another customisation you would need is to make use of the name of the sealed trait itself,
   * along with the names of the subclasses:
   *
   * Example:
   *
   *   {{{
   *     sealed trait ACredentials
   *
   *     object KinesisConfig {
   *       final case class UsernamePassword(username: String, password: String)
   *     }
   *
   *     sealed trait BCredentials
   *
   *     object BCredentials {
   *       final case class UsernamePassword(username: String, password: String)
   *     }
   *
   *   final case class MyConfig(credentials: Either[ACredentials, BCredentials]
   *
   *   }}}
   *
   *  Given ,
   *
   *   {{{
   *
   *     import zio.config._
   *
   *     val myDefaultDerivation = new DeriveConfigDescriptor {
   *       def mapClassName(name: String): String = camelToSnakeCase(name)
   *       def mapFieldName(name: String): String = name
   *       val wrapSealedTraitClasses: Boolean = true
   *       val wrapSealedTraits: Boolean       = false
   *    }
   *
   *   }}}
   *
   *  If the source is HOCON, then {{{ myDefaultDerivation.descriptor[MyConfig] }}} can read:
   *
   *  {{{
   *     credentials: {
   *        username-password : {
   *           username : xyz
   *           password: xyz
   *
   *        }
   *     }
   *   }}}
   *
   *  The issue with this config design is that it is ambiguous that whether username-password represents `ACredentials`
   *  or `BCredentials`.
   *
   *  The way to solve this problem is by setting wrapSealedTraits to true
   *
   *  Given,
   *
   *  {{{
   *     import zio.config._
   *
   *     val betterDerivation = new DeriveConfigDescriptor {
   *       def mapClassName(name: String): String = camelToSnakeCase(name)
   *       def mapFieldName(name: String): String = name
   *       val wrapSealedTraitClasses: Boolean = true
   *       val wrapSealedTraits: Boolean       = true
   *    }
   *  }}}
   *
   *   If the source is HOCON, then {{{ betterDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *     credentials: {
   *       a-credentials {
   *          username-password : {
   *             username : xyz
   *             password: xyz
   *          }
   *        }
   *     }
   *   }}}
   *
   */
  def wrapSealedTraits: Boolean

  import zio.config.ConfigDescriptor._

  implicit val implicitStringDesc: Descriptor[String]                   = Descriptor(string)
  implicit val implicitBooleanDesc: Descriptor[Boolean]                 = Descriptor(boolean)
  implicit val implicitByteDesc: Descriptor[Byte]                       = Descriptor(byte)
  implicit val implicitShortDesc: Descriptor[Short]                     = Descriptor(short)
  implicit val implicitIntDesc: Descriptor[Int]                         = Descriptor(int)
  implicit val implicitLongDesc: Descriptor[Long]                       = Descriptor(long)
  implicit val implicitBigIntDesc: Descriptor[BigInt]                   = Descriptor(bigInt)
  implicit val implicitFloatDesc: Descriptor[Float]                     = Descriptor(float)
  implicit val implicitDoubleDesc: Descriptor[Double]                   = Descriptor(double)
  implicit val implicitBigDecimalDesc: Descriptor[BigDecimal]           = Descriptor(bigDecimal)
  implicit val implicitUriDesc: Descriptor[URI]                         = Descriptor(uri)
  implicit val implicitUrlDesc: Descriptor[URL]                         = Descriptor(url)
  implicit val implicitScalaDurationDesc: Descriptor[ScalaDuration]     = Descriptor(duration)
  implicit val implicitDurationDesc: Descriptor[Duration]               = Descriptor(zioDuration)
  implicit val implicitUUIDDesc: Descriptor[UUID]                       = Descriptor(uuid)
  implicit val implicitLocalDateDesc: Descriptor[LocalDate]             = Descriptor(localDate)
  implicit val implicitLocalTimeDesc: Descriptor[LocalTime]             = Descriptor(localTime)
  implicit val implicitLocalDateTimeDesc: Descriptor[LocalDateTime]     = Descriptor(localDateTime)
  implicit val implicitInstantDesc: Descriptor[Instant]                 = Descriptor(instant)
  implicit val implicitFileDesc: Descriptor[File]                       = Descriptor(file)
  implicit val implicitJavaFilePathDesc: Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  implicit def implicitListDesc[A: Descriptor]: Descriptor[List[A]] =
    Descriptor(listDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitSetDesc[A: Descriptor]: Descriptor[Set[A]] =
    Descriptor(setDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitMapDesc[K, A: Descriptor]: Descriptor[Map[String, A]] =
    Descriptor(mapDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitEitherDesc[A: Descriptor, B: Descriptor]: Descriptor[Either[A, B]] =
    Descriptor(eitherDesc(implicitly[Descriptor[A]].desc, implicitly[Descriptor[B]].desc))

  implicit def implicitOptionDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(optionDesc(implicitly[Descriptor[A]].desc))

  protected def listDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[List[A]] =
    list(desc)

  protected def setDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Set[A]] =
    set(desc)

  protected def mapDesc[A](
    desc: ConfigDescriptor[A]
  ): ConfigDescriptor[Map[String, A]] =
    map(desc)

  protected def eitherDesc[A, B](
    left: ConfigDescriptor[A],
    right: ConfigDescriptor[B]
  ): ConfigDescriptor[Either[A, B]] =
    left.orElseEither(right)

  protected def optionDesc[A](configDesc: ConfigDescriptor[A]): ConfigDescriptor[Option[A]] =
    configDesc.optional

  case class Descriptor[T](desc: ConfigDescriptor[T], isObject: Boolean = false) {
    final def ??(description: String): Descriptor[T] =
      describe(description)

    def default(value: T): Descriptor[T] =
      Descriptor(desc.default(value))

    def describe(description: String): Descriptor[T] =
      Descriptor(desc.describe(description))

    def from(that: ConfigSource): Descriptor[T] =
      Descriptor(desc.from(that))

    def transform[B](f: T => B, g: B => T): Descriptor[B] =
      xmap(f, g)

    def transformEither[B](f: T => Either[String, B], g: B => Either[String, T]): Descriptor[B] =
      xmapEither(f, g)

    def transformEitherLeft[B](f: T => Either[String, B], g: B => T): Descriptor[B] =
      Descriptor(desc.transformEitherLeft(f, g))

    def transformEitherLeft[E, B](f: T => Either[E, B])(g: B => T)(h: E => String): Descriptor[B] =
      Descriptor(desc.transformEitherLeft[E, B](f)(g)(h))

    def transformEitherRight[E, B](f: T => B, g: B => Either[String, T]): Descriptor[B] =
      Descriptor(desc.transformEitherRight(f, g))

    def transformEitherRight[E, B](f: T => B)(g: B => Either[E, T])(h: E => String): Descriptor[B] =
      Descriptor(desc.transformEitherRight[E, B](f)(g)(h))

    def xmap[B](f: T => B, g: B => T): Descriptor[B] =
      Descriptor(desc.xmap(f, g))

    def xmapEither[B](f: T => Either[String, B], g: B => Either[String, T]): Descriptor[B] =
      Descriptor(desc.xmapEither(f, g))

    def xmapEither[E, B](f: T => Either[E, B])(g: B => Either[E, T])(h: E => String): Descriptor[B] =
      Descriptor(desc.xmapEither[E, B](f)(g)(h))
  }

  object Descriptor {
    def apply[A](implicit ev: Descriptor[A]): Descriptor[A] = ev

    sealed trait ClassNameStrategy

    object ClassNameStrategy {
      final case object WrapClassName         extends ClassNameStrategy
      final case object IgnoreClassName       extends ClassNameStrategy
      final case object ClassNameWithInConfig extends ClassNameStrategy
    }
  }

  type Typeclass[T] = Descriptor[T]

  def toSnakeCase(name: String): String = DerivationUtils.toSnakeCase(name)

  final def wrapSealedTrait[T](
    label: String,
    desc: ConfigDescriptor[T]
  ): ConfigDescriptor[T] =
    if (wrapSealedTraits) nested(label)(desc)
    else desc

  final def prepareClassName(annotations: Seq[Any], name: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(mapClassName(name))

  final def prepareFieldName(annotations: Seq[Any], name: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(mapFieldName(name))

  final def combine[T](caseClass: CaseClass[Descriptor, T]): Descriptor[T] = {
    val descriptions = caseClass.annotations.collect { case d: describe => d.describe }
    val ccName       = prepareClassName(caseClass.annotations, caseClass.typeName.short)

    val res =
      if (caseClass.isObject) constant[T](ccName, caseClass.construct(_ => ???))
      else
        caseClass.parameters.toList match {
          case Nil =>
            constantString(ccName).xmap[T](
              _ => caseClass.construct(_ => ???),
              _ => ccName
            )
          case head :: tail =>
            def makeDescriptor(param: Param[Descriptor, T]): ConfigDescriptor[Any] = {
              val descriptions =
                param.annotations
                  .filter(_.isInstanceOf[describe])
                  .map(_.asInstanceOf[describe].describe)

              val paramName = prepareFieldName(param.annotations, param.label)

              val raw = param.typeclass.desc

              val withDefaults = param.default.fold(raw)(raw.default(_))

              val described = descriptions.foldLeft(withDefaults)(_ ?? _)

              nested(paramName)(described).asInstanceOf[ConfigDescriptor[Any]]
            }

            collectAll(makeDescriptor(head), tail.map(makeDescriptor): _*).xmap[T](
              l => caseClass.rawConstruct(l),
              t => caseClass.parameters.map(_.dereference(t)).toList
            )
        }

    Descriptor(descriptions.foldLeft(res)(_ ?? _), caseClass.isObject || caseClass.parameters.isEmpty)
  }

  final def dispatch[T](sealedTrait: SealedTrait[Descriptor, T]): Descriptor[T] = {
    val nameToLabel =
      sealedTrait.subtypes
        .map(tc => prepareClassName(tc.annotations, tc.typeName.short) -> tc.typeName.full)
        .groupBy(_._1)
        .toSeq
        .flatMap {
          case (label, Seq((_, fullName))) => (fullName -> label) :: Nil
          case (label, seq) =>
            seq.zipWithIndex.map { case ((_, fullName), idx) => fullName -> s"${label}_$idx" }
        }
        .toMap

    val desc =
      sealedTrait.subtypes.map { subtype =>
        val typeclass: Descriptor[subtype.SType] = subtype.typeclass
        val desc =
          if (typeclass.isObject || !wrapSealedTraitClasses) typeclass.desc
          else nested(nameToLabel(subtype.typeName.full))(typeclass.desc)

        wrapSealedTrait(prepareClassName(sealedTrait.annotations, sealedTrait.typeName.short), desc).xmapEither[T](
          st => Right(st),
          t =>
            subtype.cast
              .andThen(Right(_))
              .applyOrElse(t, (_: T) => Left(s"Expected ${subtype.typeName.full}, but got ${t.getClass.getName}"))
        )
      }.reduce(_.orElse(_))

    Descriptor(desc)
  }

  /**
   * By default this method is not implicit to allow custom non-recursive derivation
   * */
  implicit def getDescriptor[T]: Descriptor[T] = macro Magnolia.gen[T]

  def descriptor[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    config.desc
}
