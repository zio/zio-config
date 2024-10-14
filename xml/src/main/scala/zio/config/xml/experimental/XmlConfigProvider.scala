package zio.config.xml.experimental

import zio.ConfigProvider
import zio.config.IndexedFlat.ConfigPath

object XmlConfigProvider {

  /**
   * Retrieve a `ConfigProvider` from xml string.
   */
  def fromXmlString(string: String): ConfigProvider =
    XmlParser.parse(string) match {
      case Left(value)  =>
        throw new Exception(s"Failed to parse xml string. Please make sure the format is correct. ${value}")
      case Right(value) =>
        ConfigProvider.fromMap(value.flattened.map { case (k, v) => ConfigPath.toPath(k).mkString(".") -> v })
    }

}
