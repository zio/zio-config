package zio.config.examples
import zio.config.Config
import zio.config.ConfigDescriptor._
import zio.console.Console
import zio.{ config, console, App, ZEnv, ZIO, ZLayer }

/**
 * An example of an entire application that uses java properties
 */
final case class ApplicationConfig(bridgeIp: String, userName: String)

object ApplicationConfig {
  val configuration =
    ((string("bridgeIp")) |@| string("username"))(ApplicationConfig.apply, ApplicationConfig.unapply)
}

// The main App
object SimpleExampleMain extends App {

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val pgm =
      for {
        fileLocation <- ZIO.effect(System.getProperty("user.home") + "/somefile.properties")
        // there are many ways of doing this: example: {{{ read(configuration from ConfigSource.fromJavaProperties(propertyFile))) }}}, you may try that as well.
        configLayer = Config.fromPropertiesFile(fileLocation, ApplicationConfig.configuration)
        _           <- SimpleExample.finalExecution.provideLayer(configLayer ++ ZLayer.requires[Console])
      } yield ()

    pgm.foldM(
      throwable => console.putStr(throwable.getMessage()) *> ZIO.succeed(1),
      _ => console.putStrLn("hurray !! Application ran successfully..") *> ZIO.succeed(0)
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
