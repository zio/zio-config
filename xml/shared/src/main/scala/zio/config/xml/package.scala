package zio.config

import zio._

package object xml {
  implicit class FromConfigSourceXml(c: ConfigProvider.type) {
    def fromYamlString(xml: String): ConfigProvider =
      XmlConfigProvider.fromXmlString(xml)
  }
}
