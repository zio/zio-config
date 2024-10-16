package zio.config.magnolia

import zio.config._
import zio.NonEmptyChunk

import java.io.File
import java.net.{URI, URL}
import java.time.{Duration, Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValue, summonFrom, constValueTuple}
import scala.quoted
import scala.util.Try
import DeriveConfig._
import zio.{Config, ConfigProvider, LogLevel, Chunk}, Config._
import zio.config.syntax._
import zio.config.derivation._
import scala.annotation.nowarn

final case class DeriveConfig[A](desc: Config[A], metadata: Option[DeriveConfig.Metadata] = None) {
  def ??(description: String): DeriveConfig[A] =
    describe(description)

  def describe(description: String): DeriveConfig[A] =
    DeriveConfig(desc.??(description))

  def map[B](f: A => B): DeriveConfig[B] =
    DeriveConfig(desc.map(f))

  def mapAttempt[B](f: A => B): DeriveConfig[B] =
    DeriveConfig(desc.mapAttempt(f))

  def mapOrFail[B](f: A => Either[Config.Error, B]): DeriveConfig[B] =
    DeriveConfig(desc.mapOrFail(f))
}

object DeriveConfig {

  def apply[A](using ev: DeriveConfig[A]): DeriveConfig[A] =
    ev

  def from[A](desc: Config[A]) =
    DeriveConfig(desc, None)

  sealed trait Metadata {
    def originalName: String = this match {
      case Metadata.Object(name, _)       => name.originalName
      case Metadata.Product(name, _, _)   => name.originalName
      case Metadata.Coproduct(name, _, _) => name.originalName
    }

    def alternativeNames: List[String] = this match {
      case Metadata.Object(_, _)          => Nil
      case Metadata.Product(name, _, _)   => name.alternativeNames
      case Metadata.Coproduct(name, _, _) => name.alternativeNames
    }
  }

  object Metadata {
    final case class Object[T](name: ProductName, constValue: T) extends Metadata
    final case class Product(name: ProductName, fields: List[FieldName], keyModifiers: List[KeyModifier])
        extends Metadata
    final case class Coproduct(name: CoproductName, metadata: List[Metadata], keyModifiers: List[KeyModifier])
        extends Metadata
  }

  final case class FieldName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class ProductName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class CoproductName(
    originalName: String,
    alternativeNames: List[String],
    descriptions: List[String],
    typeDiscriminator: Option[String]
  )

  lazy given DeriveConfig[Secret]         = DeriveConfig.from(secret)
  lazy given DeriveConfig[OffsetDateTime] = DeriveConfig.from(offsetDateTime)
  lazy given DeriveConfig[LogLevel]       = DeriveConfig.from(logLevel)
  lazy given DeriveConfig[String]         = DeriveConfig.from(string)
  lazy given DeriveConfig[Boolean]        = DeriveConfig.from(boolean)
  lazy given DeriveConfig[Int]            = DeriveConfig.from(int)
  lazy given DeriveConfig[BigInt]         = DeriveConfig.from(bigInt)
  lazy given DeriveConfig[Float]          = DeriveConfig.from(float)
  lazy given DeriveConfig[Double]         = DeriveConfig.from(double)
  lazy given DeriveConfig[BigDecimal]     = DeriveConfig.from(bigDecimal)
  lazy given DeriveConfig[URI]            = DeriveConfig.from(uri)
  lazy given DeriveConfig[zio.Duration]   = DeriveConfig.from(duration)
  lazy given DeriveConfig[LocalDate]      = DeriveConfig.from(localDate)
  lazy given DeriveConfig[LocalTime]      = DeriveConfig.from(localTime)
  lazy given DeriveConfig[LocalDateTime]  = DeriveConfig.from(localDateTime)
  lazy given DeriveConfig[Byte]           = DeriveConfig(Config.byte)
  lazy given DeriveConfig[Short]          = DeriveConfig(Config.short)
  lazy given DeriveConfig[UUID]           = DeriveConfig(Config.uuid)
  lazy given DeriveConfig[Long]           = DeriveConfig(Config.long)

  given optDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Option[A]] =
    DeriveConfig.from(ev.desc.optional)

  given eitherConfig[A, B](using evA: DeriveConfig[A], evB: DeriveConfig[B]): DeriveConfig[Either[A, B]] =
    DeriveConfig.from(evA.desc.orElseEither(evB.desc))

  given listDesc[A](using ev: DeriveConfig[A]): DeriveConfig[List[A]] =
    DeriveConfig.from(listOf(ev.desc))

  given seqDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Seq[A]] =
    DeriveConfig.from(listOf(ev.desc).map(_.toSeq))

  given setDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Set[A]] =
    DeriveConfig.from(setOf(ev.desc))

  given vectorDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Vector[A]] =
    DeriveConfig.from(vectorOf(ev.desc))

  given chunkDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Chunk[A]] =
    DeriveConfig.from(chunkOf(ev.desc))

  given mapDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Map[String, A]] =
    DeriveConfig.from(table(ev.desc))

  inline def summonDeriveConfigForCoProduct[T <: Tuple]: List[DeriveConfig[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  =>
        val desc = summonInline[DeriveConfig[t]]
        DeriveConfig[Any](
          desc.desc,
          desc.metadata
        ) :: summonDeriveConfigForCoProduct[ts]

  inline def summonDeriveConfigAll[T <: Tuple]: List[DeriveConfig[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  =>
        summonInline[DeriveConfig[t]] :: summonDeriveConfigAll[ts]

  inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].toString :: labelsOf[ts]

  inline def customNamesOf[T]: List[String] =
    AnnotationMacros.nameOf[T].map(_.name)

  inline def customFieldNamesOf[T]: Map[String, name] =
    AnnotationMacros.fieldNamesOf[T].flatMap { case (str, nmes) => nmes.map(name => (str, name)) }.toMap

  inline given derived[T](using m: Mirror.Of[T]): DeriveConfig[T] =
    lazy val keyModifiers =
      (AnnotationMacros.keyModifiers[T] ++ AnnotationMacros.caseModifier[T])
        .map:
          case p: prefix          => KeyModifier.Prefix(p.prefix)
          case p: postfix @nowarn => KeyModifier.Postfix(p.postfix)
          case p: suffix          => KeyModifier.Suffix(p.suffix)
          case _: kebabCase       => KeyModifier.KebabCase
          case _: snakeCase       => KeyModifier.SnakeCase

    inline m match
      case s: Mirror.SumOf[T] =>
        val coproductName: CoproductName =
          CoproductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = AnnotationMacros.descriptionOf[T].map(_.describe),
            typeDiscriminator = AnnotationMacros.discriminatorOf[T].headOption.map(_.keyName)
          )

        lazy val subClassDescriptions =
          summonDeriveConfigForCoProduct[m.MirroredElemTypes]

        lazy val desc =
          mergeAllProducts(subClassDescriptions.map(castTo[DeriveConfig[T]]), coproductName.typeDiscriminator)

        DeriveConfig.from(tryAllKeys(desc.desc, None, coproductName.alternativeNames, keyModifiers))

      case m: Mirror.ProductOf[T] =>
        val productName =
          ProductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = AnnotationMacros.descriptionOf[T].map(_.describe)
          )

        lazy val originalFieldNamesList =
          labelsOf[m.MirroredElemLabels]

        lazy val customFieldNameMap =
          customFieldNamesOf[T]

        lazy val documentations =
          AnnotationMacros.fieldDescriptionsOf[T].toMap

        lazy val fieldAndDefaultValues: Map[String, Any] =
          DefaultValueMacros.defaultValuesOf[T].toMap

        lazy val fieldNames =
          originalFieldNamesList.foldRight(Nil: List[FieldName]) { (str, list) =>
            val alternativeNames = customFieldNameMap.get(str).map(v => List(v.name)).getOrElse(Nil)
            val descriptions     = documentations.get(str).map(_.map(_.describe)).getOrElse(Nil)
            FieldName(str, alternativeNames.toList, descriptions) :: list
          }

        lazy val fieldConfigs =
          summonDeriveConfigAll[m.MirroredElemTypes].asInstanceOf[List[DeriveConfig[Any]]]

        lazy val fieldConfigsWithDefaultValues =
          addDefaultValues(fieldAndDefaultValues, originalFieldNamesList, fieldConfigs)

        mergeAllFields(
          fieldConfigsWithDefaultValues,
          productName,
          fieldNames,
          keyModifiers,
          lst => m.fromProduct(Tuple.fromArray(lst.toArray[Any])),
          castTo[Product](_).productIterator.toList
        )

  def mergeAllProducts[T](
    allDescs: => List[DeriveConfig[T]],
    typeDiscriminator: Option[String]
  ): DeriveConfig[T] =

    val desc =
      typeDiscriminator match {
        case None =>
          allDescs
            .map(desc =>
              desc.metadata match {
                case Some(Metadata.Product(productName, fields, keyModifiers)) if (fields.nonEmpty) =>
                  tryAllKeys(desc.desc, Some(productName.originalName), productName.alternativeNames, keyModifiers)
                case Some(_)                                                                        => desc.desc
                case None                                                                           => desc.desc
              }
            )
            .reduce(_ orElse _)

        case Some(keyName) =>
          Config
            .string(keyName)
            .switch(
              allDescs.flatMap { desc =>
                desc.metadata match {
                  case Some(Metadata.Object(name, value)) =>
                    List(name.originalName -> Config.Constant(value.asInstanceOf[T]))

                  case Some(m) =>
                    (m.originalName :: m.alternativeNames).map(_ -> desc.desc)

                  case None => Nil
                }
              }*
            )
      }

    DeriveConfig.from(desc)

  def addDefaultValues(
    defaultValues: Map[String, Any],
    fieldNames: List[String],
    descriptors: List[DeriveConfig[Any]]
  ): List[DeriveConfig[?]] =
    descriptors.zip(fieldNames).map { case (desc, fieldName) =>
      defaultValues.get(fieldName) match {
        case Some(any) => DeriveConfig(desc.desc.withDefault(any), desc.metadata)
        case None      => desc
      }
    }

  def mergeAllFields[T](
    allDescs: => List[DeriveConfig[?]],
    productName: ProductName,
    fieldNames: => List[FieldName],
    keyModifiers: List[KeyModifier],
    f: List[Any] => T,
    g: T => List[Any]
  ): DeriveConfig[T] =
    if fieldNames.isEmpty then // if there are no fields in the product then the value is the name of the product itself
      val tryAllPaths =
        (productName.originalName :: productName.alternativeNames)
          .map(n => zio.Config.constant(n))
          .reduce(_ orElse _)

      DeriveConfig(
        tryAllPaths.map[T](_ => f(Nil)),
        Some(Metadata.Object[T](productName, f(Nil))) // We propogate the info that product was actually an object
      )
    else
      val listOfDesc =
        fieldNames.zip(allDescs).map { case (fieldName, desc) =>
          val fieldDesc = tryAllKeys(desc.desc, Some(fieldName.originalName), fieldName.alternativeNames, keyModifiers)
          fieldName.descriptions.foldRight(fieldDesc)((doc, desc) => desc ?? doc)
        }

      val descOfList =
        Config.collectAll(listOfDesc.head, listOfDesc.tail*)

      DeriveConfig(descOfList.map(f), Some(Metadata.Product(productName, fieldNames, keyModifiers)))

  def tryAllKeys[A](
    desc: Config[A],
    originalKey: Option[String],
    alternativeKeys: List[String],
    keyModifiers: List[KeyModifier]
  ): Config[A] =

    val sortedKeyModifiers = keyModifiers.sortWith {
      case (a: CaseModifier, b: CaseModifier) => false
      case (a: CaseModifier, _)               => false
      case (_, b: CaseModifier)               => true
      case _                                  => false
    }

    val modifyKey: String => String =
      sortedKeyModifiers.map(KeyModifier.getModifierFunction).foldLeft(_)((key, modifier) => modifier(key))

    alternativeKeys match {
      case Nil  =>
        originalKey.fold(desc)(k => desc.nested(modifyKey(k)))
      case keys => keys.view.map(k => desc.nested(modifyKey(k))).reduce(_ orElse _)
    }

  def castTo[T](a: Any): T =
    a.asInstanceOf[T]
}
