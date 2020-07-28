package zio.config.magnolia

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import magnolia._
import zio.config.derivation.DerivationUtils._
import zio.duration.Duration

import scala.concurrent.duration.{ Duration => ScalaDuration }
import scala.language.experimental.macros
import zio.config._

trait DeriveConfigDescriptor { self =>
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

  /**
   *
   * Strategy on how to name the class names in the actual config (if they are used in the config)
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
   *  Given:
   *
   *   {{{
   *     import zio.config._
   *
   *     val customDerivation = new DeriveConfigDescriptor {
   *         override def mapClassName(name: String): String = camelToKebab(name)
   *     }
   *
   *     // Usage:
   *     customDerivation.descriptor[MyConfig]
   *
   *   }}}
   *
   *  If the source is HOCON, then {{{ customDerivation.descriptor[MyConfig] }}} can read:
   *
   *   {{{
   *     auth : {
   *       username-password : {
   *          username : xyz
   *          password : abc
   *
   *       }
   *     }
   *
   *   }}}
   *
   *   Alternative solution:
   *
   *   {{{
   *
   *    sealed trait Credentials
   *
   *    @name("username-password")
   *    case class UsernamePassword(username: String, password: String) extends Credentials
   *
   *    @name("token")
   *    case class Token(username: String, tokenId: String) extends Credentials.
   *
   *   }}}
   *
   *   With the above structure, if the source is HOCON, the default {{{ descriptor[Credentials] }}} can read:
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
   *   The latter solution is more specific to each sealed traits.
   */
  def mapClassName(name: String): String =
    toSnakeCase(name)

  /**
   *
   *  Strategy on how to name the field names in the actual config
   *
   *   {{{
   *      val customDerivation = new DeriveConfigDescriptor {
   *        override def mapFieldName(name: String): String = name.toUpperCase
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
   *       auth : {
   *         username_password : {
   *            USERNAME : xyz
   *            PASSWORD : abc
   *         }
   *       }
   *   }}}
   */
  def mapFieldName(name: String): String =
    name

  /**
   *  Strategy to deal with sealed traits specifically.
   *
   *  Keep a note that the class names and field names are `kebab` case in these examples.
   *
   *  Suppose need to skip the use of class-names (both subclass names and name of the sealed trait in the source config),
   *  then we need the following custom derivation:
   *
   *   {{{
   *      import Descriptor.SealedTraitStrategy._
   *
   *      val customDerivation = new DeriveConfigDescriptor {
   *         override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
   *          ignoreSubClassName && ignoreSealedTraitName
   *      }
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
   *
   *   Another option is to be able to read config such as :
   *
   *   {{{
   *
   *     auth : {
   *       type     : username-password
   *       username : xyz
   *       password : abc
   *     }
   *
   *   }}}
   *
   *   In order to read the above config, we need a custom descriptor as below:
   *
   *   {{{
   *      import zio.config._
   *
   *      val customDerivation = new DeriveConfigDescriptor {
   *        override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
   *          labelSubClassName("type") && ignoreSealedTraitName
   *     }
   *   }}}
   *
   *
   * Sometimes, we have situation where we can't ignore sealedTraitName in the config.
   *
   * Example:
   *
   *   {{{
   *     sealed trait ACredentials
   *
   *     object ACredentials {
   *       final case class UsernamePassword(username: String, password: String)
   *     }
   *
   *     sealed trait BCredentials
   *
   *     object BCredentials {
   *       final case class UsernamePassword(username: String, password: String)
   *     }
   *
   *   final case class MyConfig(auth: Either[ACredentials, BCredentials]
   *
   *   }}}
   *
   *   With the default strategy which is {{ wrapSubClassName && ignoreSealedTraitName }},
   *   and given the source is HOCON, then {{{ descriptor[MyConfig] }}} can read:
   *
   *  {{{
   *     auth: {
   *        username_password : {
   *           username : xyz
   *           password: xyz
   *
   *        }
   *     }
   *   }}}
   *
   *  The issue with this config design is that it is ambiguous that whether username_password represents `ACredentials`
   *  or `BCredentials`.
   *
   *  The way to solve this problem is by specifying {{{ wrapSealedTraitName }}}
   *
   *  Given,
   *
   *  {{{
   *     import zio.config._
   *
   *     val betterDerivation = new DeriveConfigDescriptor {
   *        override def sealedTraitStrategy: Descriptor.SealedTraitStrategy =
   *          wrapSubClassName && wrapSealedTraitName
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
  import Descriptor.SealedTraitStrategy, SealedTraitStrategy._

  def sealedTraitStrategy: SealedTraitStrategy =
    wrapSubClassName && ignoreSealedTraitName

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

  type Typeclass[T] = Descriptor[T]

  final def wrapSealedTrait[T](
    label: String,
    desc: ConfigDescriptor[T]
  ): ConfigDescriptor[T] =
    sealedTraitStrategy.parentClass match {
      case Descriptor.SealedTraitNameStrategy.WrapSealedTraitName =>
        nested(label)(desc)

      case Descriptor.SealedTraitNameStrategy.IgnoreSealedTraitName =>
        desc
    }

  final def prepareClassName(annotations: Seq[Any], name: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(mapClassName(name))

  final def prepareFieldName(annotations: Seq[Any], name: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(mapFieldName(name))

  final def combine[T](caseClass: CaseClass[Descriptor, T]): Descriptor[T] = {
    val descriptions = caseClass.annotations.collect { case d: describe => d.describe }
    val ccName       = prepareClassName(caseClass.annotations, caseClass.typeName.short)

    val res =
      if (caseClass.isObject)
        constant[T](ccName, caseClass.construct(_ => ???))
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

        val subClassName =
          nameToLabel(subtype.typeName.full)

        val desc = sealedTraitStrategy.subClass match {
          case Descriptor.SealedTraitSubClassNameStrategy.IgnoreSubClassName =>
            typeclass.desc

          case Descriptor.SealedTraitSubClassNameStrategy.WrapSubClassName if typeclass.isObject =>
            typeclass.desc

          case Descriptor.SealedTraitSubClassNameStrategy.WrapSubClassName =>
            nested(subClassName)(typeclass.desc)

          case Descriptor.SealedTraitSubClassNameStrategy.LabelSubClassName(_) if typeclass.isObject =>
            typeclass.desc

          case Descriptor.SealedTraitSubClassNameStrategy.LabelSubClassName(fieldName) =>
            (string(fieldName) ?? s"Expecting a constant string ${subClassName}" |@| typeclass.desc).tupled
              .xmapEither({
                case (name, sub) =>
                  if (subClassName == name) Right(sub)
                  else Left(s"The type specified ${name} is not equal to the obtained config ${subtype.typeName.full}")
              })(b => Right((subClassName, b)): Either[String, (String, subtype.SType)])(identity)
        }

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

  implicit def getDescriptor[T]: Descriptor[T] = macro Magnolia.gen[T]

  def descriptor[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    config.desc
}

object DeriveConfigDescriptor extends DeriveConfigDescriptor
