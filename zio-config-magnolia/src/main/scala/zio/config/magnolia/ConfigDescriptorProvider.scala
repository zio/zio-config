package zio.config.magnolia

import zio.config.{ ConfigDescriptor }
import ConfigDescriptor._
import magnolia._

import scala.language.experimental.macros

trait ConfigDescriptorProvider[T] {
  def getDescription(path: String): ConfigDescriptor[String, String, T]
}

object ConfigDescriptorProvider {
  def apply[T](implicit ev: ConfigDescriptorProvider[T]): ConfigDescriptorProvider[T] = ev

  def instance[T](f: String => ConfigDescriptor[String, String, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      override def getDescription(path: String): ConfigDescriptor[String, String, T] = f(path)
    }

  implicit val stringDescriptor: ConfigDescriptorProvider[String] =
    instance(string)

  implicit val doubleDescriptor: ConfigDescriptorProvider[Double] =
    instance(double)

  implicit val intDescriptor: ConfigDescriptorProvider[Int] =
    instance(int)

  implicit def opt[A: ConfigDescriptorProvider]: ConfigDescriptorProvider[Option[A]] =
    instance(path => ConfigDescriptorProvider[A].getDescription(path).optional)

  // For automatic derivation, `Either` type in a case class implies, for the same label, it tries two descriptions and return the successful one as an Either.
  // This is equivalent to saying string("PATH").orElseEither(int("PATH")). During automatic derivations, we are unaware of alternate paths.
  implicit def eith[A: ConfigDescriptorProvider, B: ConfigDescriptorProvider]: ConfigDescriptorProvider[Either[A, B]] =
    instance(
      path =>
        ConfigDescriptorProvider[A].getDescription(path).orElseEither(ConfigDescriptorProvider[B].getDescription(path))
    )

  type Typeclass[T] = ConfigDescriptorProvider[T]

  def combine[T](caseClass: CaseClass[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(path: String): ConfigDescriptor[String, String, T] = {
        // Loop instead of map to do more things.
        def loop(seq: Seq[Param[ConfigDescriptorProvider, T]]): List[ConfigDescriptor[String, String, Any]] =
          seq.toList match {
            case Nil => Nil
            case h :: t =>
              loop(t) :+
                h.typeclass.getDescription(h.label).xmap(r => r: Any)(r => r.asInstanceOf[h.PType])
          }

        collectAll(loop(caseClass.parameters)).xmap[T](
          caseClass.rawConstruct
        )(v => caseClass.parameters.map(_.dereference(v): Any).toList)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] = ???

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  def description[T: ConfigDescriptorProvider]: ConfigDescriptor[String, String, T] =
    ConfigDescriptorProvider[T].getDescription("")
}
