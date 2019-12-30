package zio.config.magnolia

import java.net.URI
import zio.config.{ ConfigDescriptor }
import ConfigDescriptor._
import magnolia._

import scala.language.experimental.macros

trait ConfigDescriptorProvider[T] {
  def getDescription(path: Vector[String]): ConfigDescriptor[String, String, T]
}

object ConfigDescriptorProvider {
  def apply[T](implicit ev: ConfigDescriptorProvider[T]): ConfigDescriptorProvider[T] = ev

  def instance[T](f: String => ConfigDescriptor[String, String, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      override def getDescription(path: Vector[String]): ConfigDescriptor[String, String, T] = {
        def loop(
          tail: List[String],
          acc: ConfigDescriptor[String, String, T]
        ): ConfigDescriptor[String, String, T] =
          tail match {
            case Nil =>
              acc

            case h :: Nil =>
              loop(Nil, f(h))

            case h :: tail =>
              nested(h)(loop(tail, acc))
          }

        loop(
          path.toList,
          empty[String, String, T].xmapEither[T]({
            case Some(v) => Right(v)
            case None    => Left("failed - undefined state")
          })(v => Right(Some(v)))
        )
      }
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
    ConfigDescriptorProvider[A].getDescription(_).optional

  // This is equivalent to saying string("PATH").orElseEither(int("PATH")). During automatic derivations, we are unaware of alternate paths.
  implicit def eith[A: ConfigDescriptorProvider, B: ConfigDescriptorProvider]: ConfigDescriptorProvider[Either[A, B]] =
    new ConfigDescriptorProvider[Either[A, B]] {
      override def getDescription(path: Vector[String]): ConfigDescriptor[String, String, Either[A, B]] =
        ConfigDescriptorProvider[A].getDescription(path).orElseEither(ConfigDescriptorProvider[B].getDescription(path))
    }

  type Typeclass[T] = ConfigDescriptorProvider[T]

  def combine[T](caseClass: CaseClass[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(path: Vector[String]): ConfigDescriptor[String, String, T] = {
        val result: List[ConfigDescriptor[String, String, Any]] =
          caseClass.parameters.toList.map { h =>
            {
              val derivedPath = if (caseClass.isValueClass) path else path :+ h.label
              val rawDesc     = h.typeclass.getDescription(derivedPath)
              val desc =
                h.default
                  .map(r => rawDesc.default(r))
                  .getOrElse(rawDesc)

              desc.xmap(r => r: Any)(r => r.asInstanceOf[h.PType])
            }
          }

        collectAll(result).xmap[T](
          caseClass.rawConstruct
        )(v => caseClass.parameters.map(_.dereference(v): Any).toList)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(paths: Vector[String]): ConfigDescriptor[String, String, T] = {
        def loop(tail: List[Subtype[ConfigDescriptorProvider, T]]): ConfigDescriptor[String, String, T] =
          tail match {
            case Nil =>
              empty[String, String, T].xmapEither[T]({
                case Some(v) => Right(v): Either[String, T]
                case None =>
                  Left(s"Couldn't form any subtypes ${sealedTrait.subtypes.map(_.typeName.full)}"): Either[String, T]
              })(v => Right(Some(v)))
            case h :: tail => {
              val desc = h.typeclass
                .getDescription(paths)
                .xmap(r => {
                  r: T
                })(t => {
                  (t.asInstanceOf[h.SType])
                })
              if (tail.isEmpty) desc
              else desc.orElse(loop(tail))
            }
          }

        loop(sealedTrait.subtypes.toList)
      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  def description[T: ConfigDescriptorProvider]: ConfigDescriptor[String, String, T] =
    ConfigDescriptorProvider[T].getDescription(Vector.empty)
}
