package zio.config.examples

import zio.App
import zio.config._
import ConfigDescriptor._
import zio.ZIO
import zio.console.Console.Live.console._

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
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val pgm =
      for {
        fileLocation <- ZIO.effect(System.getProperty("user.home") + "/somefile.properties")
        // there are many ways of doing this: example: {{{ read(configuration from ConfigSource.fromJavaProperties(propertyFile))) }}}, you may try that as well.
        config <- Config.fromPropertyFile(fileLocation, ApplicationConfig.configuration)
        _      <- SimpleExample.finalExecution.provide(config)
      } yield ()

    pgm.foldM(
      throwable => putStr(throwable.getMessage()) *> ZIO.succeed(1),
      _ => putStrLn("hurray !! Application ran successfully..") *> ZIO.succeed(0)
    )
  }
}

// The core application functions
object SimpleExample {
  val printConfigs: ZIO[Config[ApplicationConfig], Nothing, Unit] =
    for {
      appConfig <- config[ApplicationConfig]
      s1        <- putStrLn(appConfig.bridgeIp)
      s2        <- putStrLn(appConfig.userName)
    } yield ()

  val finalExecution: ZIO[Config[ApplicationConfig], Nothing, Unit] =
    for {
      _ <- printConfigs
      _ <- putStrLn(s"processing data......")
    } yield ()
}
// A note that, with magnolia module (which is still experimental), you can skip writing the {{ configuration }} in ApplicationConfig object
// import zio.config.magnolia.ConfigDescriptorProvider_,
// val configuration = description[ApplicationConfig], and requires case class names as configuration keys
