package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object yaml {
  implicit class FromConfigSourceYaml(c: ConfigProvider.type) {
    def fromYamlFile(file: File): ConfigProvider =
      YamlConfigProvider.fromYamlFile(file)

    def fromYamlPath(path: Path): ConfigProvider =
      YamlConfigProvider.fromYamlPath(path)

    def fromYamlReader(
      reader: Reader
    ): ConfigProvider =
      YamlConfigProvider.fromYamlReader(reader)

    def fromYamlString(
      yamlString: String
    ): ConfigProvider =
      YamlConfigProvider.fromYamlString(yamlString)

    def fromYamlRepr[A](repr: A)(
      loadYaml: A => AnyRef
    ): ConfigProvider =
      YamlConfigProvider.getIndexedConfigProvider(loadYaml(repr))
  }

}
