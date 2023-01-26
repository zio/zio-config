package zio.config.magnolia

import zio.config._
import zio.config.derivation.DerivationUtils
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
import Descriptor._
import zio.Config, Config._

final case class Descriptor[A](desc: Config[A], metadata: Option[Descriptor.Metadata])

object Descriptor {
  def apply[A](implicit ev: Descriptor[A]): Config[A] =
    ev.desc

  def from[A](desc: Config[A]) =
    Descriptor(desc, None)

  sealed trait Metadata

  object Metadata {
    final case class Object(name: ProductName) extends Metadata
    final case class Product(name: ProductName, fields: List[FieldName]) extends Metadata
    final case class Coproduct(name: CoproductName, metadata: List[Metadata]) extends Metadata
  }

  final case class FieldName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class ProductName(originalName: String, alternativeNames: List[String], descriptions: List[String])
  final case class CoproductName(originalName: String, alternativeNames: List[String], descriptions: List[String])

  lazy given Descriptor[String] = Descriptor.from(string)
  lazy given Descriptor[Boolean] = Descriptor.from(boolean)
  lazy given Descriptor[Int] = Descriptor.from(int)
  lazy given Descriptor[BigInt] = Descriptor.from(bigInt)
  lazy given Descriptor[Float] = Descriptor.from(float)
  lazy given Descriptor[Double] = Descriptor.from(double)
  lazy given Descriptor[BigDecimal] = Descriptor.from(bigDecimal)
  lazy given Descriptor[URI] = Descriptor.from(uri)
  lazy given Descriptor[zio.Duration] = Descriptor.from(duration)
  lazy given Descriptor[LocalDate] = Descriptor.from(localDate)
  lazy given Descriptor[LocalTime] = Descriptor.from(localTime)
  lazy given Descriptor[LocalDateTime] = Descriptor.from(localDateTime)

  given eitherDesc[A, B](using ev1: Descriptor[A], ev2: Descriptor[B]): Descriptor[Either[A, B]] =
    Descriptor.from(ev1.desc.orElseEither(ev2.desc))

  given optDesc[A](using ev: Descriptor[A]): Descriptor[Option[A]] =
    Descriptor.from(ev.desc.optional)

  given listDesc[A](using ev: Descriptor[A]): Descriptor[List[A]] =
    Descriptor.from(list(ev.desc))

  given mapDesc[A](using ev: Descriptor[A]): Descriptor[Map[String, A]] =
    Descriptor.from(map(ev.desc))

  given nonEmptyChunkDesc[A](using ev: Descriptor[A]): Descriptor[NonEmptyChunk[A]] =
    Descriptor.from(nonEmptyChunk(ev.desc))

  inline def summonDescriptorForCoProduct[T <: Tuple]: List[Descriptor[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val desc = summonInline[Descriptor[t]]
        Descriptor[Any](
          desc.desc, desc.metadata
        ) :: summonDescriptorForCoProduct[ts]

  inline def summonDescriptorAll[T <: Tuple]: List[Descriptor[_]] =
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[Descriptor[t]] :: summonDescriptorAll[ts]

  inline def labelsOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsOf[ts]

  inline def customNamesOf[T]: List[String] =
    Macros.nameOf[T].map(_.name) ++ Macros.namesOf[T].flatMap(_.names)

  inline def customFieldNamesOf[T]: Map[String, names] =
    (Macros.fieldNameOf[T].map({ case(str, nmes) => (str, names.fromListOfName(nmes)) })
      ++ Macros.fieldNamesOf[T].map({ case(str, nmes) => (str, names.fromListOfNames(nmes)) })).toMap

  inline given derived[T](using m: Mirror.Of[T]): Descriptor[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        val coproductName: CoproductName =
          CoproductName(
            originalName = constValue[m.MirroredLabel],
            alternativeNames = customNamesOf[T],
            descriptions = Macros.documentationOf[T].map(_.describe)
          )

        lazy val subClassDescriptions =
          summonDescriptorForCoProduct[m.MirroredElemTypes]

        lazy val desc =
          mergeAllProducts(subClassDescriptions.map(castTo[Descriptor[T]]))

        Descriptor.from(tryAllkeys(desc.desc, None, coproductName.alternativeNames))

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
            val alternativeNames = customFieldNameMap.get(str).map(_.names).getOrElse(Nil)
            val descriptions = documentations.get(str).map(_.map(_.describe)).getOrElse(Nil)
            FieldName(str, alternativeNames.toList, descriptions) :: list
          })

        lazy val descriptors =
          summonDescriptorAll[m.MirroredElemTypes].asInstanceOf[List[Descriptor[Any]]]

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
    allDescs: => List[Descriptor[T]],
  ): Descriptor[T] =
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

    Descriptor.from(desc)

  def addDefaultValues(
    defaultValues: Map[String, Any],
    fieldNames: List[String],
    descriptors: List[Descriptor[Any]]
  ): List[Descriptor[_]] = {
    descriptors.zip(fieldNames).map({ case (desc, fieldName) =>
      defaultValues.get(fieldName) match {
        case Some(any) => Descriptor(desc.desc.withDefault(any), desc.metadata)
        case None => desc
    }})
  }

  def mergeAllFields[T](
    allDescs: => List[Descriptor[_]],
    productName: ProductName,
    fieldNames: => List[FieldName],
    f: List[Any] => T,
    g: T => List[Any],
   ): Descriptor[T] =
       if fieldNames.isEmpty then
         val tryAllPaths =
           (productName.originalName :: productName.alternativeNames)
             .map(n => zio.Config.succeed(n)).reduce(_ orElse _)

         Descriptor(
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
           collectAll(listOfDesc.head, listOfDesc.tail: _*)

         Descriptor(descOfList.transform[T](f, g), Some(Metadata.Product(productName, fieldNames)))

  def tryAllkeys[A](
    desc: Config[A],
    originalKey: Option[String],
    alternativeKeys: List[String]
  ): Config[A] =
    if alternativeKeys.nonEmpty then
      alternativeKeys.map(key => nested(key)(desc)).reduce(_ orElse _)
    else
      originalKey.fold(desc)(key => nested(key)(desc))

  def castTo[T](a: Any): T =
    a.asInstanceOf[T]

  extension[E, A](e: Either[E, A]) {
    def mapError(f: E => String) =
      e.swap.map(f).swap
  }
}
