package zio.config.magnolia

import java.net.URI
import zio.config.{ ConfigDescriptor }
import ConfigDescriptor._
import magnolia._

import scala.language.experimental.macros
import scala.reflect.macros.TypecheckException
import scala.tools.nsc.doc.model.TypeClassConstraint

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
    ConfigDescriptorProvider[A].getDescription(_).optional

  implicit def listt[A: ConfigDescriptorProvider]: ConfigDescriptorProvider[List[A]] =
    a => list(ConfigDescriptorProvider[A].getDescription(a))

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

              val desc =
                h.default
                  .map(r => rawDesc.default(r))
                  .getOrElse(rawDesc)

              desc.xmap(r => r: Any)(r => r.asInstanceOf[h.PType])
            }
          }

        val resultx =
          collectAll(::(result.head, result.tail)).xmap[T](cons => {
            caseClass.rawConstruct(cons.reverse)
          })(v => {
            val r = caseClass.parameters.map(_.dereference(v): Any).toList; ::(r.head, r.tail)
          })

        if (path.isEmpty) resultx else nested(path)(resultx)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(paths: String): ConfigDescriptor[String, String, T] = {
        val list         = sealedTrait.subtypes.toList
        val head :: tail = ::(list.head, list.tail)

        tail.foldLeft(
          head.typeclass
            .getDescription(paths)
            .xmapEither(t => Right(t: T))({
              case sType: head.SType => Right(sType)
              case _                 => Left("failed")
            })
        )(
          (a, b) =>
            a.orElse(
              b.typeclass
                .getDescription(paths)
                .xmapEither(
                  t => Right(t: T)
                )({
                  case sType: b.SType => Right(sType)
                  case _              => Left("failed")
                })
            )
        )
      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  def description[T: ConfigDescriptorProvider]: ConfigDescriptor[String, String, T] =
    ConfigDescriptorProvider[T].getDescription("")
}
