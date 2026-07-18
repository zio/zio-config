package zio.config.toml

import org.tomlj.{Toml, TomlArray, TomlParseResult, TomlTable}
import zio.config.IndexedFlat.{ConfigPath, KeyComponent}
import zio.{Chunk, ConfigProvider, Task, ZIO}

import java.io.{File, Reader}
import java.nio.file.Path
import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime}
import scala.jdk.CollectionConverters._

object TomlConfigProvider {

  /**
   * Retrieve a `ConfigProvider` from a TOML file.
   */
  def fromTomlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromTomlPath(file.toPath, enableCommaSeparatedValueAsList)

  /**
   * Retrieve a `ConfigProvider` from a path to a TOML file.
   */
  def fromTomlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(path), enableCommaSeparatedValueAsList)

  /**
   * Retrieve a `ConfigProvider` from a TOML reader.
   */
  def fromTomlReader(reader: Reader, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(reader), enableCommaSeparatedValueAsList)

  /**
   * Retrieve a `ConfigProvider` from a TOML string.
   */
  def fromTomlString(input: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromToml(Toml.parse(input), enableCommaSeparatedValueAsList)

  /**
   * Retrieve a `ConfigProvider` from a parsed TomlJ result.
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

  def fromTomlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlFile(file, enableCommaSeparatedValueAsList))

  def fromTomlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlPath(path, enableCommaSeparatedValueAsList))

  def fromTomlReaderZIO(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromTomlReader(reader, enableCommaSeparatedValueAsList))

  def fromTomlStringZIO(input: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromTomlString(input, enableCommaSeparatedValueAsList))

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
