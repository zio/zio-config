package zio.config.xml.experimental

import com.github.ghik.silencer.silent
import zio.ConfigProvider
import zio.config.IndexedFlat.ConfigPath
import zio.config._

@silent("Unused import")
object XmlConfigProvider {

  /**
   * Retrieve a `ConfigProvider` from xml string.
   */
  def fromXmlString(string: String): ConfigProvider =
    XmlParser.parse(string) match {
      case Left(value)  =>
        throw new Exception(s"Failed to parse xml string. Please make sure the format is correct. ${value}")
      case Right(value) =>
        ConfigProvider.fromIndexedMap(value.flattened.map({ case (k, v) => ConfigPath.toPath(k).mkString(".") -> v }))
    }

}
