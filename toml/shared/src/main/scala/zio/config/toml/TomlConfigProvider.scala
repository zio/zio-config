package zio.config.toml

import org.tomlj.{Toml, TomlArray, TomlParseResult, TomlTable}
import zio.config.IndexedFlat.{ConfigPath, KeyComponent}
import zio.{Chunk, ConfigProvider, Task, ZIO}

import java.io.{File, Reader}
import java.nio.file.Path
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import scala.jdk.CollectionConverters._

/**
 * Builds a `ConfigProvider` from TOML, parsed with the tomlj library. `import zio.config.toml._` adds the string, file,
 * path and reader constructors to `ConfigProvider` as well, as `ConfigProvider.fromTomlString` and so on.
 *
 * Each constructor parses its input when it is called. It throws the `java.nio.file.NoSuchFileException` from tomlj
 * when a file is missing, and a `RuntimeException` whose message starts with `Unable to parse TOML:` when the TOML has
 * syntax errors. Each constructor has a `...ZIO` variant that returns a `Task[ConfigProvider]` and fails the task
 * instead.
 *
 * A table, inline table or dotted key becomes nested keys, an array or array of tables becomes a list, and an empty
 * array is an empty list. Date and time values are passed on as the `toString` of their `java.time` value, such as
 * `2026-11-17` or `10:30` for `10:30:00`, so they load with `Config.localDate`, `Config.localTime` and the like. An
 * array placed directly inside another array keeps only its last element: `a = [ [1, 2], [3, 4] ]` read with
 * `Config.listOf("a", Config.listOf(Config.int))` gives `List(List(2), List(4))`.
 *
 * By default a string value is never split. With `enableCommaSeparatedValueAsList = true`, a string value read as a
 * list is split at each `,`, so `ports = "8080,8443"` read with `Config.listOf("ports", Config.int)` gives
 * `List(8080, 8443)`. TOML arrays are lists with or without the flag.
 *
 * {{{
 * import zio._
 *
 * final case class Backend(url: String, weight: Int)
 *
 * val backends: Config[List[Backend]] =
 *   Config.listOf("backends", (Config.string("url") zip Config.int("weight")).map { case (url, weight) =>
 *     Backend(url, weight)
 *   })
 *
 * val loaded: IO[Config.Error, List[Backend]] =
 *   TomlConfigProvider.fromTomlString("""
 * [[backends]]
 * url = "http://a.internal"
 * weight = 3
 *
 * [[backends]]
 * url = "http://b.internal"
 * weight = 1
 * """).load(backends)
 * // List(Backend(http://a.internal,3), Backend(http://b.internal,1))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-config/read-from-various-sources/ Config providers]]
 */
object TomlConfigProvider {

  /**
   * Builds a `ConfigProvider` from a TOML file. Throws the `java.nio.file.NoSuchFileException` from tomlj when the file
   * does not exist, and a `RuntimeException` whose message starts with `Unable to parse TOML:` when the file has syntax
   * errors.
   */
  def fromTomlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromTomlPath(file.toPath, enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from the TOML file at `path`, the same way as `fromTomlFile`.
   */
  def fromTomlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(path), enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from the TOML text that `reader` supplies. Throws a `RuntimeException` whose message
   * starts with `Unable to parse TOML:` when the text has syntax errors.
   */
  def fromTomlReader(reader: Reader, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(reader), enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from a TOML string. Throws a `RuntimeException` whose message starts with
   * `Unable to parse TOML:` and lists tomlj's errors, separated by `; `, when the string has syntax errors.
   *
   * {{{
   * import zio._
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
  def fromTomlString(input: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(input), enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from a `TomlParseResult` that you parsed with tomlj, for example to check it first.
   * Throws a `RuntimeException` whose message starts with `Unable to parse TOML:` when `result.hasErrors` is `true`.
   */
  def fromToml(result: TomlParseResult, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider = {
    if (result.hasErrors) {
      val messages = result.errors().asScala.map(_.toString).mkString("; ")
      throw new RuntimeException(s"Unable to parse TOML: $messages")
    }

    lazy val hiddenDelim = "\uFEFF"

    val indexedMapWithHiddenDelimiter =
      getIndexedMap(result).map { case (key, value) =>
        ConfigPath.toPath(key).mkString(hiddenDelim) -> value
      }

    ConfigProvider.fromMap(
      indexedMapWithHiddenDelimiter,
      pathDelim = hiddenDelim,
      seqDelim = if (enableCommaSeparatedValueAsList) "," else hiddenDelim
    )
  }

  /**
   * Same as `fromTomlFile`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTomlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlFile(file, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromTomlPath`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTomlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlPath(path, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromTomlReader`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTomlReaderZIO(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromTomlReader(reader, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromTomlString`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTomlStringZIO(input: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlString(input, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromToml`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTomlZIO(
    result: TomlParseResult,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromToml(result, enableCommaSeparatedValueAsList))

  private[config] def getIndexedMap(input: TomlTable): Map[Chunk[KeyComponent], String] = {
    def loopNumber(path: Chunk[KeyComponent], value: Number)   = Map(path -> value.toString)
    def loopBoolean(path: Chunk[KeyComponent], value: Boolean) = Map(path -> value.toString)
    def loopString(path: Chunk[KeyComponent], value: String)   = Map(path -> value)

    def loopArray(path: Chunk[KeyComponent], values: TomlArray): Map[Chunk[KeyComponent], String] =
      if (values.isEmpty) {
        Map(path -> "<nil>")
      } else {
        (0 until values.size()).map { i =>
          loopAny(path :+ KeyComponent.Index(i), values.get(i))
        }.reduceOption(_ ++ _).getOrElse(Map.empty)
      }

    def loopTable(path: Chunk[KeyComponent], table: TomlTable): Map[Chunk[KeyComponent], String] =
      table
        .entrySet()
        .asScala
        .toVector
        .flatMap { entry =>
          val newPath = path :+ KeyComponent.KeyName(entry.getKey)
          val result  = loopAny(newPath, entry.getValue)
          if (result.isEmpty) Map(newPath -> "") else result
        }
        .toMap

    def loopAny(path: Chunk[KeyComponent], value: AnyRef): Map[Chunk[KeyComponent], String] =
      value match {
        case table: TomlTable           => loopTable(path, table)
        case array: TomlArray           => loopArray(path, array)
        case string: String             => loopString(path, string)
        case boolean: java.lang.Boolean => loopBoolean(path, boolean.booleanValue())
        case number: Number             => loopNumber(path, number)
        case dateTime: OffsetDateTime   => loopString(path, dateTime.toString)
        case dateTime: LocalDateTime    => loopString(path, dateTime.toString)
        case date: LocalDate            => loopString(path, date.toString)
        case time: LocalTime            => loopString(path, time.toString)
        case other                      =>
          throw new RuntimeException(s"Unsupported TOML value at path ${ConfigPath.toPath(path).mkString(".")}: $other")
      }

    loopTable(Chunk.empty, input)
  }

}
