package zio.config.magnolia

import java.net.URI
import zio.config.{ ConfigDescriptor }
import ConfigDescriptor._
import magnolia._
import scala.util.Success
import scala.util.Failure
import scala.language.experimental.macros

trait ConfigDescriptorProvider[T] {
  def getDescription(path: String): ConfigDescriptor[String, String, T]
}

object ConfigDescriptorProvider {
  def apply[T](implicit ev: ConfigDescriptorProvider[T]): ConfigDescriptorProvider[T] = ev

  def instance[T](f: String => ConfigDescriptor[String, String, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      override def getDescription(path: String): ConfigDescriptor[String, String, T] =
        f(path)
    }

  implicit val stringDesc: ConfigDescriptorProvider[String]         = instance(string)
  implicit val booleanDesc: ConfigDescriptorProvider[Boolean]       = instance(boolean)
  implicit val byteDesc: ConfigDescriptorProvider[Byte]             = instance(byte)
  implicit val shortDesc: ConfigDescriptorProvider[Short]           = instance(short)
  implicit val intDesc: ConfigDescriptorProvider[Int]               = instance(int)
  implicit val longDesc: ConfigDescriptorProvider[Long]             = instance(long)
  implicit val bigIntDesc: ConfigDescriptorProvider[BigInt]         = instance(bigInt)
  implicit val floatDesc: ConfigDescriptorProvider[Float]           = instance(float)
  implicit val doubleDesc: ConfigDescriptorProvider[Double]         = instance(double)
  implicit val bigDecimalDesc: ConfigDescriptorProvider[BigDecimal] = instance(bigDecimal)
  implicit val uriDesc: ConfigDescriptorProvider[URI]               = instance(uri)

  implicit def opt[A: ConfigDescriptorProvider]: ConfigDescriptorProvider[Option[A]] =
    a => ConfigDescriptorProvider[A].getDescription(a).optional

  implicit def listt[A: ConfigDescriptorProvider]: ConfigDescriptorProvider[List[A]] =
    a => list(ConfigDescriptorProvider[A].getDescription(a))

  implicit def nonEmptyList[A: ConfigDescriptorProvider]: ConfigDescriptorProvider[::[A]] =
    a =>
      list(ConfigDescriptorProvider[A].getDescription(a)).xmapEither(
        list =>
          list.headOption match {
            case Some(value) => Right(::(value, list.tail))
            case None =>
              Left(
                "The list is empty. Either provide a non empty list, and if not mark it as optional and choose to avoid it in the config"
              )
          }
      )(nonEmpty => Right(nonEmpty.toList))

  // This is equivalent to saying string("PATH").orElseEither(int("PATH")). During automatic derivations, we are unaware of alternate paths.
  implicit def eith[A: ConfigDescriptorProvider, B: ConfigDescriptorProvider]: ConfigDescriptorProvider[Either[A, B]] =
    new ConfigDescriptorProvider[Either[A, B]] {
      override def getDescription(path: String): ConfigDescriptor[String, String, Either[A, B]] =
        ConfigDescriptorProvider[A].getDescription(path).orElseEither(ConfigDescriptorProvider[B].getDescription(path))
    }

  type Typeclass[T] = ConfigDescriptorProvider[T]

  def combine[T](caseClass: CaseClass[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(path: String): ConfigDescriptor[String, String, T] = {
        val result: List[ConfigDescriptor[String, String, Any]] =
          caseClass.parameters.toList.map { h =>
            {
              val rawDesc =
                h.typeclass.getDescription(h.label)

              val descriptions =
                h.annotations
                  .filter(_.isInstanceOf[describe])
                  .map(_.asInstanceOf[describe].describe)

              val desc =
                h.default
                  .map(r => rawDesc.default(r))
                  .getOrElse(rawDesc)

              val withDocs =
                updateConfigWithDocuments(descriptions, desc)

              withDocs.xmap(r => r: Any)(r => r.asInstanceOf[h.PType])
            }
          }

        val finalDesc =
          collectAll(::(result.head, result.tail)).xmap[T](cons => {
            caseClass.rawConstruct(cons)
          })(v => {
            val r = caseClass.parameters.map(_.dereference(v): Any).toList
            ::(r.head, r.tail)
          })

        val annotations = caseClass.annotations
          .filter(_.isInstanceOf[zio.config.magnolia.describe])
          .map(_.asInstanceOf[describe].describe)

        val withDocsCaseClass =
          updateConfigWithDocuments(annotations, finalDesc)

        if (path.isEmpty) withDocsCaseClass else nested(path)(withDocsCaseClass)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(paths: String): ConfigDescriptor[String, String, T] = {
        val list         = sealedTrait.subtypes.toList
        val head :: tail = ::(list.head, list.tail)

        tail.foldRight(
          head.typeclass
            .getDescription(paths)
            .xmapEither(t => Right(t: T))({ a =>
              scala.util.Try(head.cast(a)) match {
                case Success(value) => Right(value)
                case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
              }
            })
        )(
          (e, b) =>
            b.orElse(
              e.typeclass
                .getDescription(paths)
                .xmapEither(
                  t => Right(t: T)
                )({ a =>
                  scala.util.Try(e.cast(a)) match {
                    case Success(value) => Right(value)
                    case Failure(value) => Left(s"Failure when trying to write: ${value.getMessage}")
                  }
                })
            )
        )

      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  private[config] def updateConfigWithDocuments[K, V, A](
    documents: Seq[String],
    config: ConfigDescriptor[K, V, A]
  ) =
    documents.foldLeft(config)((cf, doc) => cf ?? doc)

  def description[T: ConfigDescriptorProvider]: ConfigDescriptor[String, String, T] =
    ConfigDescriptorProvider[T].getDescription("")
}
