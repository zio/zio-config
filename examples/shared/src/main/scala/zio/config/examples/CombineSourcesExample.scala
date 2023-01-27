package zio.config.examples

import com.github.ghik.silencer.silent
import zio._
import zio.config._
import zio.config.magnolia._
import zio.config.typesafe._

import java.io.File

/**
 * One of the ways you can summon various sources especially
 * when some of the `fromSource` functions return ZIO.
 * The below example will return error result, as all the sources
 * are invalid.
 */
object CombineSourcesExample extends ZIOAppDefault {
  override def run: URIO[Any, ExitCode] =
    application.either.flatMap(r => Console.printLine(s"Result: ${r}")).exitCode

  final case class Config(username: String, password: String)

  @silent("deprecated")
  val config: Config[Config] =
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

  val application: ZIO[Any, String, String] =
    for {
      configValue <- read(config).mapError(_.prettyPrint())
      string      <- ZIO.fromEither(configValue.toJson(config))
    } yield string
}
