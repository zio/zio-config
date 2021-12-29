package zio.config.examples

import zio.{ZIO, Has}

import zio.config._, ConfigDescriptor._
import zio.console._

object LayerExample extends App {

  // Simple
  final case class MyConfig(age: Int, name: String)

  object MyConfig {
    val config =
      (int("age") zip string("name")).to[MyConfig] from ConfigSource.fromMap(Map("age" -> "20", "name" -> "afsal"))
  }

  val app: ZIO[Has[MyConfig] with zio.console.Console, java.io.IOException, Unit] =
    getConfig[MyConfig].flatMap(age => putStrLn(s"My age is ${age}"))

  val io: ZIO[zio.console.Console, Exception, Unit] =
    app.provideSomeLayer[Console](configLayer_(MyConfig.config))

  println(zio.Runtime.default.unsafeRun(io))

}
