package zio.config.magnolia

import magnolia._
import zio.Duration
import zio.config._
import zio.config.derivation.DerivationUtils._

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import ConfigDescriptorAdt._

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
   * Map all keys that correspond to either the name of a sealed-trait or
   * the name of a case-classe
   */
  def mapClassName(f: String => String): Descriptor[T] =
    Descriptor(desc.mapSubClassName(f).mapSealedTraitName(f))

  /**
   * Map all keys, where key corresponds to only field names of a case class,
   * and not the one corresponding the name of any class
   */
  def mapFieldName(f: String => String): Descriptor[T] =
    Descriptor(desc.mapFieldName(f))

  /**
   * Map all keys with a given function `f: String => String`
   */
  def mapKey(f: String => String): Descriptor[T] =
    Descriptor(desc.mapKey(f))

  /**
   * Remove the need of all keys that corresponds to the name of sealed-trait
   */
  def removeSealedTraitNameKey =
    Descriptor(desc.removeKey(ConfigDescriptorAdt.KeyType.SealedTrait))

  /**
   * Remove the need of all keys that corresponds to the name of a subclass of a sealed-trait
   */
  def removeSubClassNameKey =
    Descriptor(desc.removeKey(ConfigDescriptorAdt.KeyType.SubClass))

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

object Descriptor {
  // The default behaviour of zio-config is to discard the name of a sealed trait
  def apply[A](implicit ev: Descriptor[A]): Descriptor[A] =
    ev.removeSealedTraitNameKey

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
    labels: Seq[String],
    desc: ConfigDescriptor[T]
  ): ConfigDescriptor[T] = {
    val f = (name: String) => nested(name)(desc, Some(ConfigDescriptorAdt.KeyType.SealedTrait))
    labels.tail.foldLeft(f(labels.head)) { case (acc, n) =>
      acc orElse f(n)
    }
  }

  final def prepareClassName(annotations: Seq[Any], name: String): String       =
    annotations.collectFirst { case d: name => d.name }.getOrElse(name)

  final def prepareClassNames(annotations: Seq[Any], name: String): Seq[String] =
    annotations.collectFirst { case d: names => d.names }.getOrElse(List[String]()) ++
      List(annotations.collectFirst { case d: name => d.name }.getOrElse(name))

  final def prepareFieldName(annotations: Seq[Any], name: String): String       =
    annotations.collectFirst { case d: name => d.name }.getOrElse(name)

  final def prepareFieldNames(annotations: Seq[Any], name: String): Seq[String] =
    annotations.collectFirst { case d: names => d.names }.getOrElse(List[String]()) ++
      List(annotations.collectFirst { case d: name => d.name }.getOrElse(name))

  final def combine[T](caseClass: CaseClass[Descriptor, T]): Descriptor[T] = {
    val descriptions = caseClass.annotations.collect { case d: describe => d.describe }
    val ccNames      = prepareClassNames(caseClass.annotations, caseClass.typeName.short)

    val res =
      caseClass.parameters.toList match {
        case Nil          =>
          val f = (name: String) =>
            constantString(name).transform[T](
              _ => caseClass.construct(_ => ???),
              _ => name
            )
          ccNames.tail.foldLeft(f(ccNames.head)) { case (acc, n) =>
            acc orElse f(n)
          }
        case head :: tail =>
          def makeNestedParam(name: String, unwrapped: ConfigDescriptor[Any], optional: Boolean) =
            if (optional) {
              nested(name)(unwrapped).optional.asInstanceOf[ConfigDescriptor[Any]]
            } else {
              nested(name)(unwrapped)
            }

          def makeDescriptor(param: Param[Descriptor, T]): ConfigDescriptor[Any] = {
            val descriptions =
              param.annotations
                .filter(_.isInstanceOf[describe])
                .map(_.asInstanceOf[describe].describe)

            val paramNames = prepareFieldNames(param.annotations, param.label)

            val raw                      = param.typeclass.desc
            val (unwrapped, wasOptional) = unwrapFromOptional(raw)
            val withNesting              = paramNames.tail.foldLeft(makeNestedParam(paramNames.head, unwrapped, wasOptional)) {
              case (acc, name) =>
                acc orElse makeNestedParam(name, unwrapped, wasOptional)
            }
            val described                = descriptions.foldLeft(withNesting)(_ ?? _)
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
          case (label, seq)                =>
            seq.zipWithIndex.map { case ((_, fullName), idx) => fullName -> s"${label}_$idx" }
        }
        .toMap

    val desc =
      sealedTrait.subtypes.map { subtype =>
        val typeclass: Descriptor[subtype.SType] = subtype.typeclass

        val subClassName =
          nameToLabel(subtype.typeName.full)

        val subClassNames =
          prepareClassNames(subtype.annotations, subClassName)

        val desc = {
          val keyType =
            if (typeclass.isObject)
              ConfigDescriptorAdt.KeyType.CaseObject
            else
              ConfigDescriptorAdt.KeyType.SubClass

          val f = (name: String) =>
            nested(name)(
              typeclass.desc,
              Some(keyType)
            )

          if (subClassNames.length > 1)
            subClassNames.tail.foldLeft(f(subClassNames.head)) { case (acc, n) =>
              acc orElse f(n)
            }
          else
            nested(subClassName)(
              typeclass.desc,
              Some(keyType)
            )
        }

        wrapSealedTrait(prepareClassNames(sealedTrait.annotations, sealedTrait.typeName.short), desc)
          .transformOrFail[T](
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
    descriptorWithClassNames[T]
      .removeKey(
        KeyType.SealedTrait
      )

  def descriptorForPureConfig[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    descriptorWithClassNames[T].removeKey(KeyType.SealedTrait).pureConfig("type")

  def descriptorWithClassNames[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    config.desc

  def descriptorWithClassNames[T](labelName: String)(implicit config: Descriptor[T]): ConfigDescriptor[T] =
    descriptorWithClassNames[T].pureConfig(labelName)

  def descriptorWithoutClassNames[T](implicit config: Descriptor[T]): ConfigDescriptor[T] =
    descriptorWithClassNames[T].removeKey(
      KeyType.SealedTrait,
      KeyType.SubClass
    )
}
