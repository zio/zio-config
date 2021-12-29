package zio.config.examples

import com.github.ghik.silencer.silent
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._
import zio.console.putStrLn
import zio.{App, ExitCode, URIO, ZIO}

import java.io.File

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

  @silent("deprecated")
  val config: ConfigDescriptor[Config] =
    (descriptor[Config] from
      TypesafeConfigSource
        .fromHoconFile(new File("/invalid/path"))
        .<>(
          TypesafeConfigSource.fromHoconString(s"")
        )
        .<>(
          ConfigSource.fromSystemEnv()
        )
        .<>(
          ConfigSource.fromSystemProps()
        ))

  val application: ZIO[zio.system.System with zio.console.Console, String, String] =
    for {
      configValue <- read(config).mapError(_.prettyPrint())
      string      <- ZIO.fromEither(configValue.toJson(config))
    } yield string
}
