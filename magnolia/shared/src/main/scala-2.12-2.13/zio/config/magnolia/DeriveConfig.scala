package zio.config.magnolia

import magnolia._
import zio.config._
import zio.{Chunk, NonEmptyChunk, Config, LogLevel}

import java.net.URI
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import java.util.UUID
import scala.collection.immutable

final case class DeriveConfig[T](desc: Config[T], isObject: Boolean = false, constValue: Option[T] = None) {

  def ??(description: String): DeriveConfig[T] =
    describe(description)

  def describe(description: String): DeriveConfig[T] =
    DeriveConfig(desc.??(description))

  def map[B](f: T => B): DeriveConfig[B] =
    DeriveConfig(desc.map(f))

  def mapAttempt[B](f: T => B): DeriveConfig[B] =
    DeriveConfig(desc.mapAttempt(f))

  def mapOrFail[B](f: T => Either[Config.Error, B]): DeriveConfig[B] =
    DeriveConfig(desc.mapOrFail(f))

}

object DeriveConfig {
  def apply[A](implicit ev: DeriveConfig[A]): DeriveConfig[A] =
    ev

  import Config._

  implicit val implicitSecretDesc: DeriveConfig[Secret]                 = DeriveConfig(secret)
  implicit val implicitOffsetDateTimeDesc: DeriveConfig[OffsetDateTime] = DeriveConfig(offsetDateTime)
  implicit val implicitLogLevelDesc: DeriveConfig[LogLevel]             = DeriveConfig(logLevel)
  implicit val implicitStringDesc: DeriveConfig[String]                 = DeriveConfig(string)
  implicit val implicitBooleanDesc: DeriveConfig[Boolean]               = DeriveConfig(boolean)
  implicit val implicitIntDesc: DeriveConfig[Int]                       = DeriveConfig(int)
  implicit val implicitBigIntDesc: DeriveConfig[BigInt]                 = DeriveConfig(bigInt)
  implicit val implicitFloatDesc: DeriveConfig[Float]                   = DeriveConfig(float)
  implicit val implicitDoubleDesc: DeriveConfig[Double]                 = DeriveConfig(double)
  implicit val implicitBigDecimalDesc: DeriveConfig[BigDecimal]         = DeriveConfig(bigDecimal)
  implicit val implicitUriDesc: DeriveConfig[URI]                       = DeriveConfig(uri)
  implicit val implicitDurationDesc: DeriveConfig[zio.Duration]         = DeriveConfig(duration)
  implicit val implicitLocalDateDesc: DeriveConfig[LocalDate]           = DeriveConfig(localDate)
  implicit val implicitLocalTimeDesc: DeriveConfig[LocalTime]           = DeriveConfig(localTime)
  implicit val implicitLocalDateTimeDesc: DeriveConfig[LocalDateTime]   = DeriveConfig(localDateTime)
  implicit val implicitByteDesc: DeriveConfig[Byte]                     = DeriveConfig(Config.byte)
  implicit val implicitShortDesc: DeriveConfig[Short]                   = DeriveConfig(Config.short)
  implicit val implicitUUIDDesc: DeriveConfig[UUID]                     = DeriveConfig(Config.uuid)
  implicit val implicitLongDesc: DeriveConfig[Long]                     = DeriveConfig(Config.long)

  implicit def implicitOptionDesc[A: DeriveConfig]: DeriveConfig[Option[A]] =
    DeriveConfig(DeriveConfig[A].desc.optional)

  implicit def implicitEitherDesc[A: DeriveConfig, B: DeriveConfig]: DeriveConfig[Either[A, B]] =
    DeriveConfig(DeriveConfig[A].desc.orElseEither(DeriveConfig[B].desc))

  implicit def implicitListDesc[A: DeriveConfig]: DeriveConfig[List[A]] =
    DeriveConfig(Config.listOf(implicitly[DeriveConfig[A]].desc))

  implicit def implicitListSeq[A: DeriveConfig]: DeriveConfig[Seq[A]] =
    DeriveConfig(Config.listOf(implicitly[DeriveConfig[A]].desc).map(_.toSeq))

  implicit def implicitSetDesc[A: DeriveConfig]: DeriveConfig[Set[A]] =
    DeriveConfig(Config.setOf(implicitly[DeriveConfig[A]].desc))

  implicit def implicitVectorDesc[A: DeriveConfig]: DeriveConfig[Vector[A]] =
    DeriveConfig(Config.vectorOf(DeriveConfig[A].desc))

  implicit def implicitChunkDesc[A: DeriveConfig]: DeriveConfig[Chunk[A]] =
    DeriveConfig(Config.chunkOf(DeriveConfig[A].desc))

  implicit def implicitNonEmptyChunkDesc[A: DeriveConfig]: DeriveConfig[NonEmptyChunk[A]] =
    DeriveConfig(Config.nonEmptyChunkOf(DeriveConfig[A].desc))

  implicit def implicitMapDesc[A: DeriveConfig]: DeriveConfig[Map[String, A]] =
    DeriveConfig(Config.table(implicitly[DeriveConfig[A]].desc))

  type Typeclass[T] = DeriveConfig[T]

  sealed trait KeyModifier
  sealed trait CaseModifier extends KeyModifier

  object KeyModifier {
    case object KebabCase               extends CaseModifier
    case object SnakeCase               extends CaseModifier
    case object NoneModifier            extends CaseModifier
    case class Prefix(prefix: String)   extends KeyModifier
    case class Postfix(postfix: String) extends KeyModifier

    def getModifierFunction(keyModifier: KeyModifier): String => String =
      keyModifier match {
        case KebabCase        => toKebabCase
        case SnakeCase        => toSnakeCase
        case Prefix(prefix)   => addPrefixToKey(prefix)
        case Postfix(postfix) => addPostFixToKey(postfix)
        case NoneModifier     => identity
      }
  }

  final def wrapSealedTrait[T](
    labels: Seq[String],
    desc: Config[T]
  ): Config[T] = {
    val f = (name: String) => desc.nested(name)
    labels.toList match {
      case head :: _     => f(head)
      case immutable.Nil => desc
      case multiple      =>
        multiple.tail.foldLeft(f(multiple.head)) { case (acc, n) =>
          acc orElse f(n)
        }
    }

  }

  final def prepareClassName(annotations: Seq[Any], defaultClassName: String): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse(defaultClassName)

  final def prepareSealedTraitName(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case d: name => d.name }

  final def prepareFieldName(
    annotations: Seq[Any],
    name: String,
    keyModifiers: List[KeyModifier],
    caseModifier: CaseModifier
  ): String =
    annotations.collectFirst { case d: name => d.name }.getOrElse {
      val modifyKey = keyModifiers
        .foldLeft(identity[String] _) { case (allModifications, keyModifier) =>
          allModifications.andThen(KeyModifier.getModifierFunction(keyModifier))
        }
        .andThen(KeyModifier.getModifierFunction(caseModifier))
      modifyKey(name)
    }

  final def checkKeyModifier(annotations: Seq[Any]): (List[KeyModifier], CaseModifier) = {
    val modifiers = annotations.collect {
      case p: prefix  => KeyModifier.Prefix(p.prefix)
      case p: postfix => KeyModifier.Postfix(p.postfix)
    }.toList

    val caseModifier = annotations.collectFirst {
      case _: kebabCase => KeyModifier.KebabCase
      case _: snakeCase => KeyModifier.SnakeCase
    }.getOrElse(KeyModifier.NoneModifier)
    modifiers -> caseModifier
  }

  final def combine[T](caseClass: CaseClass[DeriveConfig, T]): DeriveConfig[T] = {
    val descriptions                 = caseClass.annotations.collect { case d: describe => d.describe }
    val ccName                       = prepareClassName(caseClass.annotations, caseClass.typeName.short)
    val (keyModifiers, caseModifier) = checkKeyModifier(caseClass.annotations)

    val res =
      caseClass.parameters.toList match {
        case Nil =>
          Config
            .constant(ccName)
            .map(_ => caseClass.rawConstruct(Seq.empty))

        case head :: tail =>
          def nest(name: String)(unwrapped: Config[Any]) =
            if (caseClass.isValueClass) unwrapped
            else unwrapped.nested(name)

          def makeDescriptor(param: Param[DeriveConfig, T]): Config[Any] = {
            val descriptions =
              param.annotations
                .filter(_.isInstanceOf[describe])
                .map(_.asInstanceOf[describe].describe)

            val raw         = param.typeclass.desc
            val withNesting = nest(prepareFieldName(param.annotations, param.label, keyModifiers, caseModifier))(raw)

            val described = descriptions.foldLeft(withNesting)(_ ?? _)
            param.default.fold(described)(described.withDefault(_))
          }

          Config
            .collectAll(
              Config.defer(makeDescriptor(head)),
              tail.map(a => Config.defer(makeDescriptor(a))): _*
            )
            .map[T](l => caseClass.rawConstruct(l))
      }

    val constValue =
      if (caseClass.isObject) Some(caseClass.rawConstruct(Seq.empty))
      else None

    DeriveConfig(
      descriptions.foldLeft(res)(_ ?? _),
      caseClass.isObject || caseClass.parameters.isEmpty,
      constValue
    )
  }

  final def dispatch[T](sealedTrait: SealedTrait[DeriveConfig, T]): DeriveConfig[T] = {
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

    val keyNameIfPureConfig: Option[String] =
      sealedTrait.annotations.collectFirst { case d: discriminator => d.keyName }

    val desc =
      keyNameIfPureConfig match {
        case None =>
          sealedTrait.subtypes.map { subtype =>
            val typeclass: DeriveConfig[subtype.SType] = subtype.typeclass

            val subClassName =
              nameToLabel(subtype.typeName.full)

            if (typeclass.isObject)
              typeclass.desc
            else
              typeclass.desc.nested(subClassName)
          }.reduce(_.orElse(_))

        case Some(pureConfigKeyName) =>
          Config
            .string(pureConfigKeyName)
            .switch(
              sealedTrait.subtypes.map { subtype =>
                val desc =
                  subtype.typeclass.constValue match {
                    case Some(v) => Config.Constant(v)
                    case None    => subtype.typeclass.desc
                  }
                nameToLabel(subtype.typeName.full) -> desc
              }: _*
            )
      }

    DeriveConfig(
      prepareSealedTraitName(sealedTrait.annotations).fold(desc)(name => wrapSealedTrait(List(name), desc))
    )
  }

  implicit def getDeriveConfig[T]: DeriveConfig[T] = macro Magnolia.gen[T]

  def deriveConfig[T](implicit config: DeriveConfig[T]): Config[T] =
    config.desc

}
