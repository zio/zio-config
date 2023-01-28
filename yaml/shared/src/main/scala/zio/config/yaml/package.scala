package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object yaml {
  implicit class FromConfigSourceYaml(c: ConfigProvider.type) {
    def fromYamlFile(file: File): ConfigProvider =
      YamlConfigSource.fromYamlFile(file)

    def fromYamlPath(path: Path): ConfigProvider =
      YamlConfigSource.fromYamlPath(path)

    def fromYamlReader(
      reader: Reader
    ): ConfigProvider =
      YamlConfigSource.fromYamlReader(reader)

    def fromYamlString(
      yamlString: String
    ): ConfigProvider =
      YamlConfigSource.fromYamlString(yamlString)

    def fromYamlRepr[A](repr: A)(
      loadYaml: A => AnyRef
    ): ConfigProvider =
      YamlConfigSource.convertYaml(loadYaml(repr))
  }

}
