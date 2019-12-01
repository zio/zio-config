package zio.config.magnolia

import zio.config.{ConfigDescriptor, ConfigSource}
import ConfigDescriptor._
import magnolia._
import zio.{ZIO}

import scala.language.experimental.macros

trait ConfigDescriptorTypeClass[T] {
  def getValue(path: String): ConfigDescriptor[String, String, T]
}

object ConfigDescriptorTypeClass {
  def apply[T](implicit ev: ConfigDescriptorTypeClass[T]): ConfigDescriptorTypeClass[T] = ev

  def instance[T](f: String => ConfigDescriptor[String, String, T]): ConfigDescriptorTypeClass[T] =
    new ConfigDescriptorTypeClass[T] {
      override def getValue(path: String): ConfigDescriptor[String, String, T] = f(path)
    }

  implicit val stringDescriptor: ConfigDescriptorTypeClass[String] =
    instance(string)

  implicit val doubleDescriptor: ConfigDescriptorTypeClass[Double] =
    instance(double)

  implicit val intDescriptor: ConfigDescriptorTypeClass[Int] =
    instance(int)


  type Typeclass[T] = ConfigDescriptorTypeClass[T]

  def combine[T](caseClass: CaseClass[ConfigDescriptorTypeClass, T]): ConfigDescriptorTypeClass[T] =
    new ConfigDescriptorTypeClass[T] {
      def getValue(path: String): ConfigDescriptor[String, String, T] = {
        def loop(seq: Seq[Param[ConfigDescriptorTypeClass, T]]): List[ConfigDescriptor[String, String, Any]] =
          seq.toList match {
            case Nil => Nil
            case h :: t =>
              val result = h.typeclass.getValue(h.label).xmap(r => r: Any)(r => r.asInstanceOf[h.PType])
              loop(t) :+ result
          }

        collectAll(loop(caseClass.parameters)).xmap[T](r => {
          caseClass.rawConstruct(r)
        })(v => caseClass.parameters.map(_.dereference(v): Any).toList)
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[ConfigDescriptorTypeClass, T]): ConfigDescriptorTypeClass[T] = ???

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

  def desc[T : ConfigDescriptorTypeClass]: ConfigDescriptor[String, String, SimpleConfig] =
    ConfigDescriptorTypeClass[SimpleConfig].getValue("")

}

import ConfigDescriptorTypeClass._

final case class SimpleConfig(port: Int, dburl: String, port2: Double)

object Run extends zio.App {
  // Typeclass derivation through Magnolia
  private val configDesc = desc[SimpleConfig]

  private val source =
    ConfigSource.fromMap(
      Map(
        "port" -> "1",
        "dburl" -> "some url",
        "port2" -> "3.14")
    )

  private val config = configDesc from source

  import zio.config._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    read(config).foldM(
      r => zio.console.Console.Live.console.putStrLn(r.mkString(",")) *> ZIO.succeed(1),
      result => zio.console.Console.Live.console.putStrLn(result.toString) *> ZIO.succeed(0)
    )
  }
  //
  // SimpleConfig(3,some url,1)
  //
  // Process finished with exit code 0
  //
  //
}
