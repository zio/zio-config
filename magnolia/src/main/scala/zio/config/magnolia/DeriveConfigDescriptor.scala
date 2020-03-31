package zio.config.magnolia

import java.net.URI

import magnolia._
import zio.config.ConfigDescriptor
import zio.config.ConfigDescriptor._

import scala.language.experimental.macros
import scala.util.{ Failure, Success }

trait DeriveConfigDescriptor[T] {
  def getDescription(path: String, parentClass: String): ConfigDescriptor[String, String, T]
}

object DeriveConfigDescriptor {

  def apply[T](implicit ev: DeriveConfigDescriptor[T]): DeriveConfigDescriptor[T] = ev

  def instance[T](f: String => ConfigDescriptor[String, String, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      override def getDescription(path: String, parentClass: String): ConfigDescriptor[String, String, T] =
        f(path)
    }

  implicit val stringDesc: DeriveConfigDescriptor[String]         = instance(string)
  implicit val booleanDesc: DeriveConfigDescriptor[Boolean]       = instance(boolean)
  implicit val byteDesc: DeriveConfigDescriptor[Byte]             = instance(byte)
  implicit val shortDesc: DeriveConfigDescriptor[Short]           = instance(short)
  implicit val intDesc: DeriveConfigDescriptor[Int]               = instance(int)
  implicit val longDesc: DeriveConfigDescriptor[Long]             = instance(long)
  implicit val bigIntDesc: DeriveConfigDescriptor[BigInt]         = instance(bigInt)
  implicit val floatDesc: DeriveConfigDescriptor[Float]           = instance(float)
  implicit val doubleDesc: DeriveConfigDescriptor[Double]         = instance(double)
  implicit val bigDecimalDesc: DeriveConfigDescriptor[BigDecimal] = instance(bigDecimal)
  implicit val uriDesc: DeriveConfigDescriptor[URI]               = instance(uri)

  implicit def opt[A: DeriveConfigDescriptor]: DeriveConfigDescriptor[Option[A]] =
    (a, b) => DeriveConfigDescriptor[A].getDescription(a, b).optional

  implicit def listt[A: DeriveConfigDescriptor]: DeriveConfigDescriptor[List[A]] =
    (a, b) => list(DeriveConfigDescriptor[A].getDescription(a, b))

  implicit def nonEmptyList[A: DeriveConfigDescriptor]: DeriveConfigDescriptor[::[A]] =
    (a, b) =>
      list(DeriveConfigDescriptor[A].getDescription(a, b)).xmapEither(
        list =>
          list.headOption match {
            case Some(value) => Right(::(value, list.tail))
            case None =>
              Left(
                "The list is empty. Either provide a non empty list, and if not mark it as optional and choose to avoid it in the config"
              )
          },
        ((nonEmpty: ::[A]) => Right(nonEmpty.toList))
      )

  // This is equivalent to saying string("PATH").orElseEither(int("PATH")). During automatic derivations, we are unaware of alternate paths.
  implicit def eith[A: DeriveConfigDescriptor, B: DeriveConfigDescriptor]: DeriveConfigDescriptor[Either[A, B]] =
    new DeriveConfigDescriptor[Either[A, B]] {
      override def getDescription(path: String, parentClass: String): ConfigDescriptor[String, String, Either[A, B]] =
        DeriveConfigDescriptor[A]
          .getDescription(path, parentClass)
          .orElseEither(DeriveConfigDescriptor[B].getDescription(path, parentClass))
    }

  type Typeclass[T] = DeriveConfigDescriptor[T]

  def combine[T](caseClass: CaseClass[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(path: String, parentClass: String): ConfigDescriptor[String, String, T] = {
        val result: List[ConfigDescriptor[String, String, Any]] =
          caseClass.parameters.toList.map { h =>
            {
              val rawDesc =
                h.typeclass.getDescription(h.label, "")

              val descriptions =
                h.annotations
                  .filter(_.isInstanceOf[describe])
                  .map(_.asInstanceOf[describe].describe)

              val desc =
                if (parentClass.isEmpty)
                  h.default
                    .map(r => rawDesc.default(r))
                    .getOrElse(rawDesc)
                else
                  nested(parentClass.toLowerCase())(
                    h.default
                      .map(r => rawDesc.default(r))
                      .getOrElse(rawDesc)
                  )

              val withDocs =
                updateConfigWithDocuments(descriptions, desc)

              withDocs.xmap((r: h.PType) => r: Any, (r: Any) => r.asInstanceOf[h.PType])
            }
          }

        val finalDesc =
          if (caseClass.isObject) {
            string(parentClass.toLowerCase()).xmapEither(
              str =>
                if (str == caseClass.typeName.short.toLowerCase()) Right(caseClass.rawConstruct(Seq.empty))
                else Left("cannot create instance"),
              (value: Any) => Right(value.toString.toLowerCase)
            )

          } else {
            collectAll(::(result.head, result.tail))
              .xmap[T](
                cons => caseClass.rawConstruct(cons),
                v => {
                  val r = caseClass.parameters.map(_.dereference(v): Any).toList
                  ::(r.head, r.tail)
                }
              )
          }

        val annotations = caseClass.annotations
          .filter(_.isInstanceOf[zio.config.magnolia.describe])
          .map(_.asInstanceOf[describe].describe)

        val withDocsCaseClass =
          updateConfigWithDocuments(annotations, finalDesc)

        if (path.isEmpty) withDocsCaseClass else nested(path)(withDocsCaseClass)

      }
    }

  def dispatch[T](sealedTrait: SealedTrait[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(paths: String, parentClass: String): ConfigDescriptor[String, String, T] = {
        val list         = sealedTrait.subtypes.toList
        val head :: tail = ::(list.head, list.tail)

        tail.foldRight[ConfigDescriptor[String, String, T]](
          head.typeclass
            .getDescription(paths, sealedTrait.typeName.short)
            .xmapEither(
              (t: head.SType) => Right(t: T), { a: T =>
                scala.util.Try(head.cast(a)) match {
                  case Success(value) => Right(value)
                  case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
                }
              }
            )
        )(
          (e: Subtype[Typeclass, T], b: ConfigDescriptor[String, String, T]) =>
            b.orElse(
              e.typeclass
                .getDescription(paths, sealedTrait.typeName.short.toString)
                .xmapEither(
                  (t: e.SType) => Right(t: T),
                  (a: T) =>
                    scala.util.Try(e.cast(a)) match {
                      case Success(value) => Right(value)
                      case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
                    }
                )
            )
        )

      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  private[config] def updateConfigWithDocuments[K, V, A](
    documents: Seq[String],
    config: ConfigDescriptor[K, V, A]
  ): ConfigDescriptor[K, V, A] =
    documents.foldLeft(config)((cf, doc) => cf ?? doc)

  def descriptor[T: DeriveConfigDescriptor]: ConfigDescriptor[String, String, T] =
    DeriveConfigDescriptor[T].getDescription("", "")
}
