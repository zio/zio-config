package zio.config.xml

import zio._

package object experimental {
  implicit class FromConfigSourceXml(c: ConfigProvider.type) {
    def fromYamlString(xml: String): ConfigProvider =
      XmlConfigProvider.fromXmlString(xml)
  }
}
