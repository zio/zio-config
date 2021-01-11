package zio.config.examples

import java.io.File

import zio.{ system, App, ExitCode, URIO, ZIO }
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._
import zio.console.{ putStrLn, Console }

/**
 * One of the ways you can summon various sources especially
 * when some of the `fromSource` functions return ZIO.
 * The below example will return error result, as all the sources
 * are invalid.
 */
object CombineSourcesExample extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    application.either.flatMap(r => putStrLn(s"Result: ${r}")).exitCode

  final case class Config(username: String, password: String)

  val getConfig: ZIO[system.System, ReadError[String], _root_.zio.config.ConfigDescriptor[Config]] =
    for {
      hoconFile <- ZIO.fromEither(TypesafeConfigSource.fromHoconFile(new File("/invalid/path")))
      constant  <- ZIO.fromEither(TypesafeConfigSource.fromHoconString(s""))
      env       <- ConfigSource.fromSystemEnv
      sysProp   <- ConfigSource.fromSystemProperties
      source    = hoconFile <> constant <> env <> sysProp
    } yield (descriptor[Config] from source)

  val application: ZIO[Console with system.System, String, Unit] =
    for {
      desc        <- getConfig.mapError(_.prettyPrint())
      configValue <- ZIO.fromEither(read(desc)).mapError(_.prettyPrint())
      string      <- ZIO.fromEither(configValue.toJson(desc))
      _           <- putStrLn(string)
    } yield ()
}
