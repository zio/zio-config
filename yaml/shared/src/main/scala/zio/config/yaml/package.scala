package zio.config

import zio._

import java.io.{BufferedReader, ByteArrayInputStream, File, InputStreamReader, Reader}
import java.nio.charset.Charset
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
    ): ConfigProvider = {
      val configStream = new ByteArrayInputStream(yamlString.getBytes(Charset.forName("UTF-8")))
      YamlConfigSource.fromYamlReader(new BufferedReader(new InputStreamReader(configStream)))
    }

    def fromYamlRepr[A](repr: A)(
      loadYaml: A => AnyRef
    ): ConfigProvider =
      YamlConfigSource.convertYaml(loadYaml(repr))
  }

}
