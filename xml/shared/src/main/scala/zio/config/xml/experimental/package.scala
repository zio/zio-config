package zio.config.xml

import zio._

package object experimental {

  /**
   * Adds an XML constructor to the `ConfigProvider` companion after `import zio.config.xml.experimental._`. The
   * constructor is named `fromYamlString`, but it parses XML: it is the same as `XmlConfigProvider.fromXmlString`.
   */
  implicit class FromConfigSourceXml(c: ConfigProvider.type) {

    /**
     * Parses an XML string into a `ConfigProvider`, despite its name: it is the same as
     * `XmlConfigProvider.fromXmlString`, which describes the supported XML and how it becomes keys. Throws a
     * `java.lang.Exception` when the XML can't be parsed.
     *
     * {{{
     * import zio._
     * import zio.config.xml.experimental._
     *
     * val region: IO[Config.Error, String] =
     *   ConfigProvider.fromYamlString("""<aws region="eu-west-1"></aws>""").load(Config.string("region").nested("aws"))
     * // succeeds with eu-west-1
     * }}}
     */
    def fromYamlString(xml: String): ConfigProvider =
      XmlConfigProvider.fromXmlString(xml)
  }
}
