package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object yaml {
  implicit class FromConfigYaml(c: ZConfig.type) {
    def fromPath[A: Tag](path: Path, descriptor: Config[A]): Layer[Config.Error, A] =
      YamlConfig.fromFile(path.toFile, descriptor)

    def fromFile[A: Tag](file: File, descriptor: Config[A]): Layer[Config.Error, A] =
      YamlConfig.fromFile(file, descriptor)
  }

  implicit class FromConfigSourceYaml(c: ConfigSource.type) {
    def fromYamlFile(file: File): ConfigSource =
      YamlConfigSource.fromYamlFile(file)

    def fromYamlPath(path: Path): ConfigSource =
      YamlConfigSource.fromYamlPath(path)

    def fromYamlReader(
      reader: Reader,
      sourceName: String = "yaml"
    ): ConfigSource =
      YamlConfigSource.fromYamlReader(reader, sourceName)

    def fromYamlString(
      yamlString: String,
      sourceName: String = "yaml"
    ): ConfigSource =
      YamlConfigSource.fromYamlString(yamlString, sourceName)

    def fromYamlRepr[A](repr: A)(
      loadYaml: A => ZIO[Any, Config.Error, AnyRef],
      sourceName: String = "yaml"
    ): ConfigSource =
      YamlConfigSource.fromYamlRepr(repr)(loadYaml, sourceName)
  }

}
