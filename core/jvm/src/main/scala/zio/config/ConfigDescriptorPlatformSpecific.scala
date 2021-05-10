package zio.config

import java.io.File
import java.net.{URI, URL}

trait ConfigDescriptorPlatformSpecific { self: ConfigDescriptorFunctions =>
  import ConfigDescriptorAdt._

  val file: ConfigDescriptor[File] =
    sourceDesc(ConfigSource.empty, PropertyType.FileType) ?? "value of type file"

  /**
   * A config descriptor that describes retrieving a file from a given path.
   *
   * {{{
   *
   *     val mapSource =
   *      ConfigSource.fromMap(
   *         "FILE_PATH" : "/user/file.txt"
   *      )
   *
   *     val config = file("FILE_PATH")
   *     val result = read(config from mapSource)
   *
   *     // Right(/user/file.txt)
   *
   * }}}
   */
  def file(path: String): ConfigDescriptor[File] = nested(path)(file)

  val uri: ConfigDescriptor[URI] =
    sourceDesc(ConfigSource.empty, PropertyType.UriType) ?? "value of type uri"

  /**
   * A config descriptor that describes retrieving a Uri from a given path.
   *
   * {{{
   *
   *     val mapSource =
   *      ConfigSource.fromMap(
   *         "URI" : "www.bla.com"
   *      )
   *
   *     val config = uri("URI")
   *     val result = read(config from mapSource)
   *
   *     // Right(www.bla.com)
   *
   * }}}
   */
  def uri(path: String): ConfigDescriptor[URI] = nested(path)(uri)

  val url: ConfigDescriptor[URL] =
    sourceDesc(ConfigSource.empty, PropertyType.UrlType) ?? "value of type URL"

  /**
   * A config descriptor that describes retrieving a Url from a given path.
   *
   * {{{
   *
   *     val mapSource =
   *      ConfigSource.fromMap(
   *         "URL" : "www.bla.com"
   *      )
   *
   *     val config = bigInt("COST")
   *     val result = read(config from mapSource)
   *
   *     // Right(111111111)
   *
   * }}}
   */
  def url(path: String): ConfigDescriptor[URL] = nested(path)(url)

  val javaFilePath: ConfigDescriptor[java.nio.file.Path] =
    sourceDesc(ConfigSource.empty, PropertyType.JavaFilePathType) ?? "value of type java.nio.file.Path"

  /**
   * A config descriptor that describes retrieving a javaFilePath from a given path.
   *
   * {{{
   *
   *     val mapSource =
   *      ConfigSource.fromMap(
   *         "FILE_PATH" : "/Users/abc/xyz.txt"
   *      )
   *
   *     val config = javaFilePath("COST")
   *     val result = read(config from mapSource)
   *
   *     // Right(/Users/abc/xyz.txt)
   *
   * }}}
   */
  def javaFilePath(path: String): ConfigDescriptor[java.nio.file.Path] =
    lazyDesc(nested(path)(javaFilePath))
}
