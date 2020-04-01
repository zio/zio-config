package zio.config.magnolia

import java.net.URI

import magnolia._
import zio.config.ConfigDescriptor
import zio.config.ConfigDescriptor._

import scala.language.experimental.macros
import scala.util.{ Failure, Success }

trait DeriveConfigDescriptor[T] {
  def getDescription(path: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, T]
}

object DeriveConfigDescriptor {
  def apply[T](implicit ev: DeriveConfigDescriptor[T]): ConfigDescriptor[String, String, T] =
    ev.getDescription(None, None)

  def descriptor[T](implicit ev: DeriveConfigDescriptor[T]): ConfigDescriptor[String, String, T] =
    ev.getDescription(None, None)

  def instance[T](f: String => ConfigDescriptor[String, String, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      override def getDescription(
        path: Option[String],
        parentClass: Option[String]
      ): ConfigDescriptor[String, String, T] =
        path.fold(
          string("").xmapEither[T](
            _ => Left("unable to fetch the primitive without a path"),
            (_: T) => Left("unable to write the primitive back to a config source without a path")
          )
        )(f)
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

  implicit def opt[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[Option[A]] =
    (a, b) => ev.getDescription(a, b).optional

  implicit def listt[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[List[A]] =
    (a, b) => list(ev.getDescription(a, b))

  implicit def nonEmptyList[A](implicit ev: DeriveConfigDescriptor[A]): DeriveConfigDescriptor[::[A]] =
    (a, b) =>
      list(ev.getDescription(a, b)).xmapEither(
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
      override def getDescription(
        path: Option[String],
        parentClass: Option[String]
      ): ConfigDescriptor[String, String, Either[A, B]] =
        implicitly[DeriveConfigDescriptor[A]]
          .getDescription(path, parentClass)
          .orElseEither(implicitly[DeriveConfigDescriptor[B]].getDescription(path, parentClass))
    }

  type Typeclass[T] = DeriveConfigDescriptor[T]

  def combine[T](caseClass: CaseClass[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(path: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, T] = {
        val finalDesc =
          if (caseClass.isObject) {
            val config = parentClass match {
              case Some(parentClass) =>
                path match {
                  case Some(path) => nested(parentClass)(string(path))
                  case None       => string(parentClass.toLowerCase())
                }
              case None => path.fold(string(""))(string(_))
            }
            config.xmapEither(
              str =>
                if (str == caseClass.typeName.short.toLowerCase()) Right(caseClass.rawConstruct(Seq.empty))
                else
                  Left(s"""Not enough details in the  config source to form the config using automatic derviation."""),
              (value: Any) => Right(value.toString.toLowerCase)
            )
          } else {
            val result: List[ConfigDescriptor[String, String, Any]] =
              caseClass.parameters.toList.map { h =>
                {
                  val rawDesc =
                    h.typeclass.getDescription(Some(h.label), None)

                  val descriptions =
                    h.annotations
                      .filter(_.isInstanceOf[describe])
                      .map(_.asInstanceOf[describe].describe)

                  val withDefault =
                    h.default
                      .map(r => rawDesc.default(r))
                      .getOrElse(rawDesc)

                  val withDocs =
                    updateConfigWithDocuments(descriptions, withDefault)

                  withDocs.xmap((r: h.PType) => r: Any, (r: Any) => r.asInstanceOf[h.PType])
                }
              }

            val config = collectAll(::(result.head, result.tail))
              .xmap[T](
                cons => caseClass.rawConstruct(cons),
                v => {
                  val r = caseClass.parameters.map(_.dereference(v): Any).toList
                  ::(r.head, r.tail)
                }
              )

            val withPath = path.fold(config)(nested(_)(config))

            parentClass match {
              case Some(value) =>
                nested(value.toLowerCase())(nested(caseClass.typeName.short.toLowerCase)(withPath))
              case None => withPath
            }
          }

        val annotations = caseClass.annotations
          .filter(_.isInstanceOf[zio.config.magnolia.describe])
          .map(_.asInstanceOf[describe].describe)

        updateConfigWithDocuments(annotations, finalDesc)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[DeriveConfigDescriptor, T]): DeriveConfigDescriptor[T] =
    new DeriveConfigDescriptor[T] {
      def getDescription(paths: Option[String], parentClass: Option[String]): ConfigDescriptor[String, String, T] = {
        val list         = sealedTrait.subtypes.toList
        val head :: tail = ::(list.head, list.tail)

        tail.foldRight[ConfigDescriptor[String, String, T]](
          head.typeclass
            .getDescription(paths, Some(sealedTrait.typeName.short))
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
                .getDescription(paths, Some(sealedTrait.typeName.short.toString))
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
}
