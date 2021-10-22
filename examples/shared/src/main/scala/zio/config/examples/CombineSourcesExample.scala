package zio.config.examples

import com.github.ghik.silencer.silent
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._
import zio.{Console, ExitCode, Has, System, URIO, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.File

/**
 * One of the ways you can summon various sources especially
 * when some of the `fromSource` functions return ZIO.
 * The below example will return error result, as all the sources
 * are invalid.
 */
object CombineSourcesExample extends ZIOAppDefault {
  override def run: URIO[zio.ZEnv with Has[ZIOAppArgs], ExitCode] =
    application.either.flatMap(r => Console.printLine(s"Result: ${r}")).exitCode

  final case class Config(username: String, password: String)

  @silent("deprecated")
  val getConfig: ZIO[Has[System], ReadError[String], _root_.zio.config.ConfigDescriptor[Config]] =
    for {
      hoconFile <- ZIO.fromEither(TypesafeConfigSource.fromHoconFile(new File("/invalid/path")))
      constant  <- ZIO.fromEither(TypesafeConfigSource.fromHoconString(s""))
      env       <- ConfigSource.fromSystemEnv
      sysProp   <- ConfigSource.fromSystemProperties
      source     = hoconFile <> constant <> env <> sysProp
    } yield (descriptor[Config] from source)

  val application: ZIO[Has[System] with Has[Console], String, String] =
    for {
      desc        <- getConfig.mapError(_.prettyPrint())
      configValue <- ZIO.fromEither(read(desc)).mapError(_.prettyPrint())
      string      <- ZIO.fromEither(configValue.toJson(desc))
    } yield string
}
