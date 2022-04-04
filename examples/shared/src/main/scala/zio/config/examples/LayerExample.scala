package zio.config.examples

import zio._
import zio.config._

import ConfigDescriptor._

object LayerExample extends ZIOAppDefault {

  final case class MyConfig(age: Int, name: String)

  object MyConfig {
    val config: ConfigDescriptor[MyConfig] =
      (int("age") zip string("name")).to[MyConfig] from ConfigSource.fromMap(Map("age" -> "20", "name" -> "afsal"))
  }

  val app: ZIO[MyConfig, java.io.IOException, Unit] =
    getConfig[MyConfig].flatMap(age => Console.printLine(s"My age is ${age}"))

  override def run: ZIO[MyConfig, Exception, Unit] =
    app.provide(configLayer_(MyConfig.config))

}
