package zio.config.xml.experimental

import zio.ConfigProvider
import zio.config.IndexedFlat.ConfigPath

/**
 * Builds a `ConfigProvider` from XML. It is experimental and reads a subset of XML with its own parser.
 *
 * @see
 *   [[https://zio.dev/zio-config/read-from-various-sources/ Config Providers]]
 */
object XmlConfigProvider {

  /**
   * Parses an XML string into a `ConfigProvider`. Every key path starts with the name of the root element. An attribute
   * is a key under its element, and the text of an element is the value of the element's own path, so in the example,
   * the `url` attribute and the text of the `port` element give the keys `config.database.url` and
   * `config.database.port`.
   *
   * When an attribute and a child element have the same name, the child element's value is kept. Sibling elements with
   * the same name give the same keys, and for each key the last value is kept, so repeated elements don't form a list.
   * Text with commas, such as `<hosts>a,b</hosts>`, reads as a list with `Config.listOf`. Text is trimmed of leading
   * and trailing whitespace, and character entities are kept as written, not decoded.
   *
   * The parser reads elements, attributes in double quotes, and text. It throws a `java.lang.Exception` whose message
   * starts with `Failed to parse xml string.` for input it can't read, such as an XML declaration, a comment, a CDATA
   * section, a self-closing tag, an attribute in single quotes, text that contains `=`, or text next to child elements.
   *
   * {{{
   * import zio._
   *
   * val xml: String =
   *   """|<config>
   *      |  <database url="jdbc:postgresql://localhost/app">
   *      |    <port>5432</port>
   *      |  </database>
   *      |</config>""".stripMargin
   *
   * val database: Config[(String, Int)] =
   *   Config.string("url").zip(Config.int("port")).nested("database").nested("config")
   *
   * val loaded: IO[Config.Error, (String, Int)] = XmlConfigProvider.fromXmlString(xml).load(database)
   * // succeeds with (jdbc:postgresql://localhost/app,5432)
   * }}}
   */
  def fromXmlString(string: String): ConfigProvider =
    XmlParser.parse(string) match {
      case Left(value)  =>
        throw new Exception(s"Failed to parse xml string. Please make sure the format is correct. ${value}")
      case Right(value) =>
        ConfigProvider.fromMap(value.flattened.map { case (k, v) => ConfigPath.toPath(k).mkString(".") -> v })
    }

}
