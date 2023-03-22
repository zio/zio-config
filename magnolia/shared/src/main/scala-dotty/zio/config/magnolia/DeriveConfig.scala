package zio.config.magnolia

import zio.config._
import zio.NonEmptyChunk

import java.io.File
import java.net.{URI, URL}
import java.time.{Duration, Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValue, summonFrom, constValueTuple}
import scala.quoted
import scala.util.Try
import DeriveConfig._
import zio.{Config, ConfigProvider}, Config._
import zio.config.syntax._
import zio.config.derivation._

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

  def apply[A](implicit ev: DeriveConfig[A]): DeriveConfig[A] =
    ev

  def from[A](desc: Config[A]) =
    DeriveConfig(desc, None)

  sealed trait Metadata

  object Metadata {
    final case class Object(name: ProductName) extends Metadata
    final case class Product(name: ProductName, fields: List[FieldName]) extends Metadata
    final case class Coproduct(name: CoproductName, metadata: List[Metadata]) extends Metadata
  }

  final case class FieldName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class ProductName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class CoproductName(originalName: String, alternativeNames: List[String], descriptions: List[String], typeDescriminator: Option[String])

  lazy given DeriveConfig[String] = DeriveConfig.from(string)
  lazy given DeriveConfig[Boolean] = DeriveConfig.from(boolean)
  lazy given DeriveConfig[Int] = DeriveConfig.from(int)
  lazy given DeriveConfig[BigInt] = DeriveConfig.from(bigInt)
  lazy given DeriveConfig[Float] = DeriveConfig.from(float)
  lazy given DeriveConfig[Double] = DeriveConfig.from(double)
  lazy given DeriveConfig[BigDecimal] = DeriveConfig.from(bigDecimal)
  lazy given DeriveConfig[URI] = DeriveConfig.from(uri)
  lazy given DeriveConfig[zio.Duration] = DeriveConfig.from(duration)
  lazy given DeriveConfig[LocalDate] = DeriveConfig.from(localDate)
  lazy given DeriveConfig[LocalTime] = DeriveConfig.from(localTime)
  lazy given DeriveConfig[LocalDateTime] = DeriveConfig.from(localDateTime)
  lazy given DeriveConfig[Byte] = DeriveConfig(Config.byte)
  lazy given DeriveConfig[Short] = DeriveConfig(Config.short)
  lazy given DeriveConfig[UUID] = DeriveConfig(Config.uuid)
  lazy given DeriveConfig[Long] = DeriveConfig(Config.long)

  given optDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Option[A]] =
    DeriveConfig.from(ev.desc.optional)

  given listDesc[A](using ev: DeriveConfig[A]): DeriveConfig[List[A]] =
    DeriveConfig.from(listOf(ev.desc))

  given seqDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Seq[A]] =
    DeriveConfig.from(listOf(ev.desc).map(_.toSeq))

  given mapDesc[A](using ev: DeriveConfig[A]): DeriveConfig[Map[String, A]] =
    DeriveConfig.from(table(ev.desc))

  inline def summonDeriveConfigForCoProduct[T <: Tuple]: List[DeriveConfig[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val desc = summonInline[DeriveConfig[t]]
        DeriveConfig[Any](
          desc.desc, desc.metadata
        ) :: summonDeriveConfigForCoProduct[ts]

  inline def summonDeriveConfigAll[T <: Tuple]: List[DeriveConfig[_]] =
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[DeriveConfig[t]] :: summonDeriveConfigAll[ts]

  inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsOf[ts]

  inline def customNamesOf[T]: List[String] =
    Macros.nameOf[T].map(_.name)

  inline def customFieldNamesOf[T]: Map[String, name] =
    Macros.fieldNameOf[T].flatMap({ case(str, nmes) => nmes.map(name => (str, name)) }).toMap

  inline given derived[T](using m: Mirror.Of[T]): DeriveConfig[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        val coproductName: CoproductName =
          CoproductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = Macros.documentationOf[T].map(_.describe),
            typeDescriminator = Macros.nameWithLabel[T].headOption.map(_.keyName)
          )

        lazy val subClassDescriptions =
          summonDeriveConfigForCoProduct[m.MirroredElemTypes]

        lazy val desc =
          mergeAllProducts(subClassDescriptions.map(castTo[DeriveConfig[T]]))

        DeriveConfig.from(tryAllkeys(desc.desc, None, coproductName.alternativeNames, coproductName.typeDescriminator))

      case m: Mirror.ProductOf[T] =>
        val productName =
          ProductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = Macros.documentationOf[T].map(_.describe)
          )

        lazy val originalFieldNamesList =
          labelsOf[m.MirroredElemLabels]

        lazy val customFieldNameMap =
          customFieldNamesOf[T]

        lazy val documentations =
          Macros.fieldDocumentationOf[T].toMap

        lazy val fieldAndDefaultValues: Map[String, Any] =
          Macros.defaultValuesOf[T].toMap

        lazy val fieldNames =
          originalFieldNamesList.foldRight(Nil: List[FieldName])((str, list) => {
            val alternativeNames = customFieldNameMap.get(str).map(v => List(v.name)).getOrElse(Nil)
            val descriptions = documentations.get(str).map(_.map(_.describe)).getOrElse(Nil)
            FieldName(str, alternativeNames.toList, descriptions) :: list
          })

        lazy val descriptors =
          summonDeriveConfigAll[m.MirroredElemTypes].asInstanceOf[List[DeriveConfig[Any]]]

        lazy val descriptorsWithDefaultValues =
          addDefaultValues(fieldAndDefaultValues, originalFieldNamesList, descriptors)

        mergeAllFields(
          descriptorsWithDefaultValues,
          productName,
          fieldNames,
          lst => m.fromProduct(Tuple.fromArray(lst.toArray[Any])),
          castTo[Product](_).productIterator.toList
        )

  def mergeAllProducts[T](
    allDescs: => List[DeriveConfig[T]],
  ): DeriveConfig[T] =

    val desc =
      allDescs
        .map(desc =>
          desc.metadata match {
            case Some(Metadata.Product(productName, fields)) if (fields.nonEmpty) =>
              tryAllkeys(desc.desc, Some(productName.originalName), productName.alternativeNames)

            case Some(_) => desc.desc
            case None => desc.desc
          }
        ).reduce(_ orElse _)

    DeriveConfig.from(desc)

  def addDefaultValues(
    defaultValues: Map[String, Any],
    fieldNames: List[String],
    descriptors: List[DeriveConfig[Any]]
  ): List[DeriveConfig[_]] = {
    descriptors.zip(fieldNames).map({ case (desc, fieldName) =>
      defaultValues.get(fieldName) match {
        case Some(any) => DeriveConfig(desc.desc.withDefault(any), desc.metadata)
        case None => desc
    }})
  }

  def mergeAllFields[T](
    allDescs: => List[DeriveConfig[_]],
    productName: ProductName,
    fieldNames: => List[FieldName],
    f: List[Any] => T,
    g: T => List[Any],
   ): DeriveConfig[T] =
       if fieldNames.isEmpty then
         val tryAllPaths =
           (productName.originalName :: productName.alternativeNames)
             .map(n => zio.Config.constant(n)).reduce(_ orElse _)

         DeriveConfig(
           tryAllPaths.map[T](
             _ => f(List.empty[Any])
           ),
           Some(Metadata.Object(productName))
         )

       else
         val listOfDesc =
           fieldNames.zip(allDescs).map({ case (fieldName, desc) => {
             val fieldDesc =
               tryAllkeys(castTo[Config[Any]](desc.desc), Some(fieldName.originalName), fieldName.alternativeNames)

             fieldName.descriptions.foldRight(fieldDesc)((doc, desc) => desc ?? doc)
           }})

         val descOfList =
           Config.collectAll(listOfDesc.head, listOfDesc.tail: _*)

         DeriveConfig(descOfList.map(f), Some(Metadata.Product(productName, fieldNames)))

  def tryAllkeys[A](
    desc: Config[A],
    originalKey: Option[String],
    alternativeKeys: List[String],
    typeDescriminator: Option[String]
  ): Config[A] =
    if alternativeKeys.nonEmpty then
      alternativeKeys.map(key => desc.nested(key)).reduce(_ orElse _)
    else
      originalKey.fold(desc)(key => desc.nested(key))

  def castTo[T](a: Any): T =
    a.asInstanceOf[T]

  extension[E, A](e: Either[E, A]) {
    def mapError(f: E => String) =
      e.swap.map(f).swap
  }
}
