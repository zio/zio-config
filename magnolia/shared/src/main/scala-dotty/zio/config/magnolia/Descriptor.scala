package zio.config.magnolia

import zio.config._, ConfigDescriptor._
import zio.duration.Duration
import zio.config.derivation.DerivationUtils

import java.io.File
import java.net.{URI, URL}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.UUID
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.deriving._
import scala.compiletime.{erasedValue, summonInline, constValue, summonFrom, constValueTuple}
import scala.quoted
import scala.util.Try
import Descriptor._

final case class Descriptor[A](desc: ConfigDescriptor[A], keys: Option[(ProductName, List[FieldName])] = None)

object Descriptor {
  def apply[A](desc: ConfigDescriptor[A], productName: ProductName): Descriptor[A] =
    Descriptor(desc,Some(productName -> Nil))

  def apply[A](implicit ev: Descriptor[A]) =
    ev.desc

  final case class FieldName(names: List[String])
  final case class ProductName(originalName: String, alternativeNames: List[String])
  final case class CoproductName(originalName: String, alternativeNames: List[String])

  lazy given Descriptor[String] = Descriptor(string)
  lazy given Descriptor[Boolean] = Descriptor(boolean)
  lazy given Descriptor[Byte] = Descriptor(byte)
  lazy given Descriptor[Short] = Descriptor(short)
  lazy given Descriptor[Int] = Descriptor(int)
  lazy given Descriptor[Long] = Descriptor(long)
  lazy given Descriptor[BigInt] = Descriptor(bigInt)
  lazy given Descriptor[Float] = Descriptor(float)
  lazy given Descriptor[Double] = Descriptor(double)
  lazy given Descriptor[BigDecimal] = Descriptor(bigDecimal)
  lazy given Descriptor[URI] = Descriptor(uri)
  lazy given Descriptor[URL] = Descriptor(url)
  lazy given Descriptor[ScalaDuration] = Descriptor(duration)
  lazy given Descriptor[Duration] = Descriptor(zioDuration)
  lazy given Descriptor[UUID] = Descriptor(uuid)
  lazy given Descriptor[LocalDate] = Descriptor(localDate)
  lazy given Descriptor[LocalTime] = Descriptor(localTime)
  lazy given Descriptor[LocalDateTime] = Descriptor(localDateTime)
  lazy given Descriptor[Instant] = Descriptor(instant)
  lazy given Descriptor[File] = Descriptor(file)
  lazy given Descriptor[java.nio.file.Path] = Descriptor(javaFilePath)

  given eitherDesc[A, B](using ev1: Descriptor[A], ev2: Descriptor[B]): Descriptor[Either[A, B]] =
    Descriptor(ev1.desc.orElseEither(ev2.desc))

  given optDesc[A: Descriptor]: Descriptor[Option[A]] =
    Descriptor(Descriptor[A].optional)

  given listDesc[A](using ev: Descriptor[A]): Descriptor[List[A]] =
    Descriptor(list(ev.desc))

  given mapDesc[A](using ev: Descriptor[A]): Descriptor[Map[String, A]] =
    Descriptor(map(ev.desc))

  inline def summonDescriptorForCoProduct[T <: Tuple]: List[Descriptor[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val desc = summonInline[Descriptor[t]]
        Descriptor[Any](
          desc.desc.transformOrFail[Any](
            a => Right(a),
            b =>
              Try(castTo[t](b))
                .toEither
                .mapError(r =>
                  s"Failed to write ${b}. ${r}. This happens when A gets casted to a wrong SubType (given A has multiple subtypes) during write."
                )
          ), desc.keys
        ) :: summonDescriptorForCoProduct[ts]

  inline def summonDescriptorAll[T <: Tuple]: List[Descriptor[_]] =
    inline erasedValue[T] match
      case _ : EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Descriptor[t]] :: summonDescriptorAll[ts]

  inline def labelsToList[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _ : ( t *: ts) => constValue[t].toString :: labelsToList[ts]

  inline def findAllAnnotatedNamesOf[T] =
    Macros.nameAnnotations[T].map(_.name) ++ Macros.namesAnnotations[T].flatMap(_.names)

  inline given derived[T](using m: Mirror.Of[T]): Descriptor[T] =
    inline m match
      case s: Mirror.SumOf[T] =>
        sum[T](using s)

      case p: Mirror.ProductOf[T] =>
        product[T](using p)

  inline def sum[T](using m: Mirror.SumOf[T]) =
    val nameOfT =
      constValue[m.MirroredLabel]

    val alternativeParentNames: List[String] =
      findAllAnnotatedNamesOf[T]

    val sealedTraitName: CoproductName =
      CoproductName(originalName = nameOfT, alternativeNames = alternativeParentNames)

    val subClassDescriptions =
      summonDescriptorForCoProduct[m.MirroredElemTypes]

    val desc =
      mergeAllProducts(subClassDescriptions.map(castTo[Descriptor[T]]))

    tryAllParentPaths(sealedTraitName, desc)

  def tryAllParentPaths[T](
    coproductName: CoproductName,
    desc: Descriptor[T]
  ) =
    if coproductName.alternativeNames.isEmpty then
      desc
    else
      Descriptor(coproductName.alternativeNames.map(n => nested(n)(desc.desc)).reduce(_ orElse _))

  def mergeAllProducts[T](
    allDescs: => List[Descriptor[T]],
  ): Descriptor[T] =
    Descriptor(
      allDescs
        .map(desc =>
          desc.keys match {
            case Some((productName, fields))  if (fields.nonEmpty) =>
              (productName.alternativeNames :+ productName.originalName)
                .map(n => nested(n)(desc.desc)).reduce(_ orElse _)

            case Some(_) => desc.desc
            case None => desc.desc
          }
        ).reduce(_ orElse _)
    )

  inline def product[T](using m: Mirror.ProductOf[T]) =
    val nameOfT =
      constValue[m.MirroredLabel]

    val productName =
      ProductName(nameOfT, findAllAnnotatedNamesOf[T])

    val fieldNames =
      labelsToList[m.MirroredElemLabels].map(name => FieldName(List(name)))

    mergeAllFields(
      summonDescriptorAll[m.MirroredElemTypes],
      productName,
      fieldNames,
      lst => m.fromProduct(Tuple.fromArray(lst.toArray[Any])),
      castTo[Product](_).productIterator.toList
    )

  def mergeAllFields[T](
    allDescs: => List[Descriptor[_]],
    productName: ProductName,
    fieldNames: => List[FieldName],
    f: List[Any] => T,
    g: T => List[Any],
   ): Descriptor[T] =
       if fieldNames.isEmpty then
         val tryAllPaths = (productName.originalName :: productName.alternativeNames).map(n => DerivationUtils.constantString(n)).reduce(_ orElse _)
         Descriptor(tryAllPaths.transform[T](_ => f(List.empty[Any]), t => productName.alternativeNames.headOption.getOrElse(productName.originalName)), productName)
       else
         val listOfDesc =
           fieldNames.zip(allDescs).map({ case (fieldName, desc) =>
             fieldName.names.map(name => nested(name)(castTo[ConfigDescriptor[Any]](desc.desc))).reduce(_ orElse _)
           })

         val descOfList =
           collectAll(listOfDesc.head, listOfDesc.tail: _*)

         Descriptor(descOfList.transform[T](f, g), Some(productName -> fieldNames))

  def castTo[T](a: Any): T =
    a.asInstanceOf[T]

  extension[E, A](e: Either[E, A]) {
    def mapError(f: E => String) =
      e.swap.map(f).swap
  }
}
