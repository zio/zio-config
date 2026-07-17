package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object toml {
  implicit class FromConfigSourceToml(c: ConfigProvider.type) {
    def fromTomlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TomlConfigProvider.fromTomlFile(file, enableCommaSeparatedValueAsList)

    def fromTomlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TomlConfigProvider.fromTomlPath(path, enableCommaSeparatedValueAsList)

    def fromTomlReader(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TomlConfigProvider.fromTomlReader(reader, enableCommaSeparatedValueAsList)

    def fromTomlString(
      tomlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TomlConfigProvider.fromTomlString(tomlString, enableCommaSeparatedValueAsList)

    def fromTomlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlFileZIO(file, enableCommaSeparatedValueAsList)

    def fromTomlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlPathZIO(path, enableCommaSeparatedValueAsList)

    def fromTomlReaderZIO(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlReaderZIO(reader, enableCommaSeparatedValueAsList)

    def fromTomlStringZIO(
      tomlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      TomlConfigProvider.fromTomlStringZIO(tomlString, enableCommaSeparatedValueAsList)
  }

}
