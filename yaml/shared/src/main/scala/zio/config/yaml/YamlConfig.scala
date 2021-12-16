package zio.config.yaml

import zio.config._
import zio.{IsNotIntersection, Layer, Tag}

import java.io.File
import java.nio.file.Path

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
  def fromString[A: Tag: IsNotIntersection](
    yamlString: String,
    descriptor: ConfigDescriptor[A]
  ): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(
      descriptor from YamlConfigSource.fromYamlString(yamlString)
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
  def fromPath[A: Tag: IsNotIntersection](path: Path, descriptor: ConfigDescriptor[A]): Layer[ReadError[String], A] =
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
   *     YamlConfig.fromFile(new File("/path/to/file.yml"), descriptor[MyConfig])
   * }}}
   */
  def fromFile[A: Tag: IsNotIntersection](file: File, descriptor: ConfigDescriptor[A]): Layer[ReadError[String], A] =
    ZConfig.fromConfigDescriptor(descriptor from YamlConfigSource.fromYamlFile(file))

}
