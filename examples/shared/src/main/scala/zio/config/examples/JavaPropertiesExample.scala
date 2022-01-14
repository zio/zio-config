package zio.config.examples

import zio._
import zio.config._

import java.io.IOException

import ConfigDescriptor._

/**
 * An example of an entire application that uses java properties
 */
final case class ApplicationConfig(bridgeIp: String, userName: String)

object ApplicationConfig {
  val configuration: ConfigDescriptor[ApplicationConfig] =
    ((string("bridgeIp")) zip string("username")).to[ApplicationConfig]
}

// The main App
object JavaPropertiesExample extends ZIOAppDefault {

  val properties = new java.util.Properties()
  properties.put("bridgeIp", "10.0.0.1")
  properties.put("username", "afs")

  override def run: URIO[Any with Console, ExitCode] = {
    val configLayer =
      ZConfig.fromProperties(properties, ApplicationConfig.configuration, "constant")

    val pgm =
      SimpleExample.finalExecution.provideLayer(configLayer ++ ZLayer.environment[Console])

    pgm
      .foldZIO(
        throwable => Console.print(throwable.getMessage),
        _ => Console.printLine("hurray !! Application ran successfully..")
      )
      .exitCode
  }
}

// The core application functions
object SimpleExample {

  val printConfigs: ZIO[ApplicationConfig with Console, IOException, Unit] =
    for {
      appConfig <- getConfig[ApplicationConfig]
      _         <- Console.printLine(appConfig.bridgeIp)
      _         <- Console.printLine(appConfig.userName)
    } yield ()

  val finalExecution: ZIO[ApplicationConfig with Console, IOException, Unit] =
    for {
      _ <- printConfigs
      _ <- Console.printLine(s"processing data......")
    } yield ()
}
// A note that, with magnolia module (which is still experimental), you can skip writing the {{ configuration }} in ApplicationConfig object
// import zio.config.magnolia.DeriveConfigDescriptor_,
