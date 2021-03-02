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

  /**
   *
   * Strategy on how to name the class names in the source config (if they are used in the config)
   * By default, zio-config doesn't make assumptions that config keys or class names, especially when there is
   * sealed traits or case objects
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
   *         override def mapClassName(name: String): String = toKebabCase(name)
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
   *   With the above structure, if the source is HOCON, then {{{ descriptor[Credentials] }}} can read:
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
    name

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
   **
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
   *       type     : UsernamePassword
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
   *        UsernamePassword : {
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
   *       ACredentials {
   *          UsernamePassword : {
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
            constantString(ccName).transform[T](
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

              val raw                      = param.typeclass.desc
              val (unwrapped, wasOptional) = unwrapFromOptional(raw)
              val withNesting = if (wasOptional) {
                nested(paramName)(unwrapped).optional.asInstanceOf[ConfigDescriptor[Any]]
              } else {
                nested(paramName)(unwrapped)
              }
              val described = descriptions.foldLeft(withNesting)(_ ?? _)
              param.default.fold(described)(described.default(_))
            }

            collectAll(
              ConfigDescriptorAdt.lazyDesc(makeDescriptor(head)),
              tail.map(a => ConfigDescriptorAdt.lazyDesc(makeDescriptor(a))): _*
            ).transform[T](
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
              .transformOrFail[subtype.SType](
                {
                  case (name, sub) =>
                    if (subClassName == name) Right(sub)
                    else
                      Left(s"The type specified ${name} is not equal to the obtained config ${subtype.typeName.full}")
                },
                b => Right((subClassName, b)): Either[String, (String, subtype.SType)]
              )
        }

        wrapSealedTrait(prepareClassName(sealedTrait.annotations, sealedTrait.typeName.short), desc).transformOrFail[T](
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

  /**
   * descriptor[A] allows us to automatically derive `ConfigDescriptor` instead
   * of using the ConfigDescriptor dsl explicitly (i.e, manually defining configdescriptor).
   * While manual definitions can be verbose, it is preferred when it comes to simple configurations.
   *
   * On the other hand, automatic derivation can become handly when the config is complex with relatively larger number of parameters,
   * or when it is constantly changing during the software lifecycle, or it's just complex structure with nested products and coproducts.
   * Also, when building configuration driven apps, most probably we might end up mixing manual and automatic together.
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
   * If the source is HOCON, then {{{ customDerivation.descriptor[MyConfig] }}} can read:
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
  def descriptor[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    config.desc
}

object DeriveConfigDescriptor extends DeriveConfigDescriptor {
  @deprecated("Use zio.config.magnolia.Descriptor directly", since = "1.0.1")
  val Descriptor = zio.config.magnolia.Descriptor

  @deprecated("Use zio.config.magnolia.Descriptor[T] directly", since = "1.0.1")
  type Descriptor[T] = zio.config.magnolia.Descriptor[T]
}
