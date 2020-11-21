package zio.config.yaml

import java.io.File
import java.nio.file.Path

import zio.{ Has, Layer, Tag, ZIO }
import zio.config._

object YamlConfig {

  /**
   * Retrieve a config from a Yaml string
   *
   * A complete example usage:
   *
   * {{{
   *
   *   val yamlString = ???
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     YamlConfig.fromString(yamlString, descriptor[MyConfig])
   * }}}
   */
  def fromString[A: Tag](yamlString: String, descriptor: ConfigDescriptor[A]): Layer[ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO.fromEither(YamlConfigSource.fromYamlString(yamlString).map(descriptor from _))
    )

  /**
   * Retrieve a config from a Yaml path
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     YamlConfig.fromPath(Path.of("/path/to/file.yml"), descriptor[MyConfig])
   * }}}
   */
  def fromPath[A: Tag](path: Path, descriptor: ConfigDescriptor[A]): Layer[ReadError[String], Has[A]] =
    fromFile(path.toFile, descriptor)

  /**
   * Retrieve a config from a Yaml file
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.config.magnolia.DeriveConfigDescriptor.descriptor
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Layer[ReadError[String], Has[MyConfig]] =
   *     YamlConfig.fromPath(new File("/path/to/file.yml"), descriptor[MyConfig])
   * }}}
   */
  def fromFile[A: Tag](file: File, descriptor: ConfigDescriptor[A]): Layer[ReadError[String], Has[A]] =
    ZConfig.fromConfigDescriptorM(
      ZIO.fromEither(YamlConfigSource.fromYamlFile(file).map(descriptor from _))
    )
}
