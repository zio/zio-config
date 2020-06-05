package zio.config.magnolia

import java.io.File
import java.net.{ URI, URL }
import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime }
import java.util.UUID

import magnolia._
import zio.config._
import zio.config.derivation.{ DerivationUtils, NeedsDerive }
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
  val wrapSealedTraits: Boolean       = true

  /**
   * By default this method is not implicit to allow custom non-recursive derivation
   * */
  override implicit def descriptor[T: NeedsDerive]: Descriptor[T] = macro DescriptorMacro.gen[T]
}

/**
 * Non-recursive derivation
 *
 * */
object NonRecursiveDerivation extends DeriveConfigDescriptor {
  def mapClassName(name: String): String = toSnakeCase(name)
  def mapFieldName(name: String): String = name

  val wrapSealedTraitClasses: Boolean = true
  val wrapSealedTraits: Boolean       = true
}

trait DeriveConfigDescriptor { self =>
  import zio.config.ConfigDescriptor._

  protected def stringDesc: ConfigDescriptor[String]                   = string
  protected def booleanDesc: ConfigDescriptor[Boolean]                 = boolean
  protected def byteDesc: ConfigDescriptor[Byte]                       = byte
  protected def shortDesc: ConfigDescriptor[Short]                     = short
  protected def intDesc: ConfigDescriptor[Int]                         = int
  protected def longDesc: ConfigDescriptor[Long]                       = long
  protected def bigIntDesc: ConfigDescriptor[BigInt]                   = bigInt
  protected def floatDesc: ConfigDescriptor[Float]                     = float
  protected def doubleDesc: ConfigDescriptor[Double]                   = double
  protected def bigDecimalDesc: ConfigDescriptor[BigDecimal]           = bigDecimal
  protected def uriDesc: ConfigDescriptor[URI]                         = uri
  protected def urlDesc: ConfigDescriptor[URL]                         = url
  protected def scalaDurationDesc: ConfigDescriptor[ScalaDuration]     = duration
  protected def durationDesc: ConfigDescriptor[Duration]               = zioDuration
  protected def uuidDesc: ConfigDescriptor[UUID]                       = uuid
  protected def localDateDesc: ConfigDescriptor[LocalDate]             = localDate
  protected def localTimeDesc: ConfigDescriptor[LocalTime]             = localTime
  protected def localDateTimeDesc: ConfigDescriptor[LocalDateTime]     = localDateTime
  protected def instantDesc: ConfigDescriptor[Instant]                 = instant
  protected def fileDesc: ConfigDescriptor[File]                       = file
  protected def javaFilePathDesc: ConfigDescriptor[java.nio.file.Path] = javaFilePath

  implicit val implicitStringDesc: Descriptor[String]                   = Descriptor(stringDesc)
  implicit val implicitBooleanDesc: Descriptor[Boolean]                 = Descriptor(booleanDesc)
  implicit val implicitByteDesc: Descriptor[Byte]                       = Descriptor(byteDesc)
  implicit val implicitShortDesc: Descriptor[Short]                     = Descriptor(shortDesc)
  implicit val implicitIntDesc: Descriptor[Int]                         = Descriptor(intDesc)
  implicit val implicitLongDesc: Descriptor[Long]                       = Descriptor(longDesc)
  implicit val implicitBigIntDesc: Descriptor[BigInt]                   = Descriptor(bigIntDesc)
  implicit val implicitFloatDesc: Descriptor[Float]                     = Descriptor(floatDesc)
  implicit val implicitDoubleDesc: Descriptor[Double]                   = Descriptor(doubleDesc)
  implicit val implicitBigDecimalDesc: Descriptor[BigDecimal]           = Descriptor(bigDecimalDesc)
  implicit val implicitUriDesc: Descriptor[URI]                         = Descriptor(uriDesc)
  implicit val implicitUrlDesc: Descriptor[URL]                         = Descriptor(urlDesc)
  implicit val implicitScalaDurationDesc: Descriptor[ScalaDuration]     = Descriptor(scalaDurationDesc)
  implicit val implicitDurationDesc: Descriptor[Duration]               = Descriptor(durationDesc)
  implicit val implicitUUIDDesc: Descriptor[UUID]                       = Descriptor(uuidDesc)
  implicit val implicitLocalDateDesc: Descriptor[LocalDate]             = Descriptor(localDateDesc)
  implicit val implicitLocalTimeDesc: Descriptor[LocalTime]             = Descriptor(localTimeDesc)
  implicit val implicitLocalDateTimeDesc: Descriptor[LocalDateTime]     = Descriptor(localDateTimeDesc)
  implicit val implicitInstantDesc: Descriptor[Instant]                 = Descriptor(instantDesc)
  implicit val implicitFileDesc: Descriptor[File]                       = Descriptor(fileDesc)
  implicit val implicitJavaFilePathDesc: Descriptor[java.nio.file.Path] = Descriptor(javaFilePathDesc)

  implicit def implicitListDesc[A: Descriptor]: Descriptor[List[A]] =
    Descriptor(listDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitSetDesc[A: Descriptor]: Descriptor[Set[A]] =
    Descriptor(setDesc(implicitly[Descriptor[A]].desc))

  implicit def implicitMapDesc[A: Descriptor]: Descriptor[Map[String, A]] =
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

  protected def optionDesc[A](desc: ConfigDescriptor[A]): ConfigDescriptor[Option[A]] =
    desc.optional

  case class Descriptor[T](desc: ConfigDescriptor[T], isObject: Boolean = false)

  object Descriptor {
    implicit def toConfigDescriptor[T](ev: Descriptor[T]): ConfigDescriptor[T] = ev.desc
  }

  type Typeclass[T] = Descriptor[T]

  def toSnakeCase(name: String): String = DerivationUtils.toSnakeCase(name)

  def mapClassName(name: String): String

  def mapFieldName(name: String): String

  def wrapSealedTraitClasses: Boolean

  def wrapSealedTraits: Boolean

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
        val typeclass = subtype.typeclass
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
  def descriptor[T: NeedsDerive]: Descriptor[T] = macro DescriptorMacro.gen[T]
}
