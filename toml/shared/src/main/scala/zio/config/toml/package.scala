package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object toml {

  /**
   * Adds the string, file, path and reader constructors of `TomlConfigProvider` to the `ConfigProvider` companion, so
   * that `import zio.config.toml._` makes `ConfigProvider.fromTomlString`, `ConfigProvider.fromTomlFile` and the others
   * available. Each method calls the `TomlConfigProvider` method of the same name. `TomlConfigProvider.fromToml`, which
   * takes a tomlj `TomlParseResult`, has no counterpart here.
   */
  implicit class FromConfigSourceToml(c: ConfigProvider.type) {

    /**
     * Builds a `ConfigProvider` from a TOML file. Throws a `java.nio.file.NoSuchFileException` when the file does not
     * exist, and a `RuntimeException` whose message starts with `Unable to parse TOML:` when the file has syntax
     * errors.
     */
    def fromTomlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TomlConfigProvider.fromTomlFile(file, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from the TOML file at `path`, the same way as `fromTomlFile`.
     */
    def fromTomlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TomlConfigProvider.fromTomlPath(path, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from the TOML text that `reader` supplies. Throws a `RuntimeException` whose message
     * starts with `Unable to parse TOML:` when the text has syntax errors.
     */
    def fromTomlReader(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TomlConfigProvider.fromTomlReader(reader, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from a TOML string. Throws a `RuntimeException` whose message starts with
     * `Unable to parse TOML:` and lists tomlj's errors, separated by `; `, when the string has syntax errors.
     *
     * {{{
     * import zio._
     * import zio.config.toml._
     *
     * final case class Server(host: String, ports: List[Int])
     *
     * val server: Config[Server] =
     *   (Config.string("host") zip Config.listOf("ports", Config.int)).nested("server").map { case (host, ports) =>
     *     Server(host, ports)
     *   }
     *
     * val loaded: IO[Config.Error, Server] =
     *   ConfigProvider.fromTomlString("""
     * [server]
     * host = "localhost"
     * ports = [8080, 8443]
     * """).load(server)
     * // Server(localhost,List(8080, 8443))
     * }}}
     */
    def fromTomlString(
      tomlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TomlConfigProvider.fromTomlString(tomlString, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromTomlFile`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromTomlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlFileZIO(file, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromTomlPath`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromTomlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlPathZIO(path, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromTomlReader`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromTomlReaderZIO(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlReaderZIO(reader, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromTomlString`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromTomlStringZIO(
      tomlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlStringZIO(tomlString, enableCommaSeparatedValueAsList)
  }

}
