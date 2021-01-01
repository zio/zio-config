package zio.config.typesafe

import com.typesafe.config.ConfigFactory
import zio.config._

import java.io.File
import scala.util.Try

object TypesafeConfigSource extends BaseTypesafeConfigSource {

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a given config file
   *
   * A complete example usage:
   *
   * {{{
   *   val configSource = TypesafeConfigSource.fromHoconFile(new File("/path/to/xyz.hocon"))
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Task[MyConfig] =
   *     configSource.flatMap(source => ZIO.fromEither(read(descriptor[MyConfig] from source))
   * }}}
   */
  def fromHoconFile[A](file: File): Either[ReadError[String], ConfigSource] =
    Try(ConfigFactory.parseFile(file).resolve).toEither.swap
      .map(
        r =>
          ReadError.SourceError(
            s"Unable to get a form a valid config-source from hocon file. ${r}"
          ): ReadError[String]
      )
      .swap
      .flatMap(typesafeConfig => {
        fromTypesafeConfig(typesafeConfig)
      })
}
