package zio.config.magnolia

import zio.config.{ConfigDescriptor}
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


  type Typeclass[T] = ConfigDescriptorProvider[T]

  def combine[T](caseClass: CaseClass[ConfigDescriptorProvider, T]): ConfigDescriptorProvider[T] =
    new ConfigDescriptorProvider[T] {
      def getDescription(path: String): ConfigDescriptor[String, String, T] = {
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

  def desc[T : ConfigDescriptorProvider]: ConfigDescriptor[String, String, T] =
    ConfigDescriptorProvider[T].getDescription("")
}
