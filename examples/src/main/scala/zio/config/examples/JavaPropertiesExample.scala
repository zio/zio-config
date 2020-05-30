package zio.config.examples

import zio.config.Config
import zio.config.ConfigDescriptor._
import zio.console.Console
import zio.{ config, console, App, ExitCode, ZEnv, ZIO, ZLayer }

/**
 * An example of an entire application that uses java properties
 */
final case class ApplicationConfig(bridgeIp: String, userName: String)

object ApplicationConfig {
  val configuration =
    ((string("bridgeIp")) |@| string("username"))(ApplicationConfig.apply, ApplicationConfig.unapply)
}

// The main App
object JavaPropertiesExample extends App {

  val properties = new java.util.Properties()
  properties.put("bridgeIp", "10.0.0.1")
  properties.put("username", "afs")

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val configLayer =
      Config.fromProperties(properties, ApplicationConfig.configuration, "constant")

    val pgm =
      SimpleExample.finalExecution.provideLayer(configLayer ++ ZLayer.requires[Console])

    pgm.foldM(
      throwable => console.putStr(throwable.getMessage).as(ExitCode.failure),
      _ => console.putStrLn("hurray !! Application ran successfully..").as(ExitCode.success)
    )
  }
}

// The core application functions
object SimpleExample {

  val printConfigs: ZIO[Console with Config[ApplicationConfig], Nothing, Unit] =
    for {
      appConfig <- config.config[ApplicationConfig]
      _         <- console.putStrLn(appConfig.bridgeIp)
      _         <- console.putStrLn(appConfig.userName)
    } yield ()

  val finalExecution: ZIO[Console with Config[ApplicationConfig], Nothing, Unit] =
    for {
      _ <- printConfigs
      _ <- console.putStrLn(s"processing data......")
    } yield ()
}
// A note that, with magnolia module (which is still experimental), you can skip writing the {{ configuration }} in ApplicationConfig object
// import zio.config.magnolia.DeriveConfigDescriptor_,
