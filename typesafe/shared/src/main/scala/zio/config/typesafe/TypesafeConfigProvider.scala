package zio.config.typesafe

import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.config.IndexedFlat.{ConfigPath, KeyComponent}
import zio.config._
import zio.{Chunk, ConfigProvider}
import java.lang.{Boolean => JBoolean}
import java.io.File
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

@silent("Unused import")
object TypesafeConfigProvider {

  import VersionSpecificSupport._

  /**
   * Retrieve a `ConfigProvider` from `typesafe-config` from a given file in resource classpath.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: IO[Config.Error, MyConfig] =
   *    ConfigProvider.fromResourcePath.load(deriveConfig[MyConfig])
   * }}}
   */
  def fromResourcePath: ConfigProvider =
    fromTypesafeConfig(ConfigFactory.load.resolve)

  /**
   * Retrieve a `ConfigProvider` from `typesafe-config` from a given config file
   */
  def fromHoconFile[A](file: File): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve)

  /**
   * Retrieve a `ConfigProvider` from `typesafe-config` from a path to a config file
   */
  def fromHoconFilePath[A](filePath: String): ConfigProvider =
    fromHoconFile(new File(filePath))

  /**
   * Retrieve a `ConfigProvider` from `typesafe-config` HOCON string.
   */

  def fromHoconString(input: String): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseString(input).resolve)

  def fromTypesafeConfig(config: com.typesafe.config.Config): ConfigProvider = {

    val configMap =
      getIndexedMap(config)

    ConfigProvider.fromMap(
      configMap.map { case (key, value) =>
        ConfigPath.toPath(key).mkString("\u0020") -> value
      },
      pathDelim = "\u0020"
    )
  }

  private[config] def getIndexedMap(
    input: com.typesafe.config.Config
  ): Map[Chunk[KeyComponent], String] = {
    def loopNumber(path: Chunk[KeyComponent], value: Number)           = Map(path -> value.toString)
    val loopNull                                                       = Map.empty[Chunk[KeyComponent], String]
    def loopString(path: Chunk[KeyComponent], value: String)           = Map(path -> value)
    def loopList(path: Chunk[KeyComponent], values: List[ConfigValue]) =
      if (values.isEmpty) {
        Map(path -> "<nil>")
      } else {
        values.zipWithIndex.map { case (value, i) =>
          loopAny(path :+ KeyComponent.Index(i), value)
        }.reduceOption(_ ++ _).getOrElse(Map.empty)
      }

    def loopConfig(path: Chunk[KeyComponent], config: ConfigObject): Map[Chunk[KeyComponent], String] =
      config.asScala.toVector.toMap.flatMap { case (key, value) =>
        loopAny(path :+ KeyComponent.KeyName(key), value)
      }

    def loopAny(path: Chunk[KeyComponent], value: ConfigValue): Map[Chunk[KeyComponent], String] =
      value.valueType() match {
        case ConfigValueType.OBJECT  =>
          loopConfig(path, value.asInstanceOf[ConfigObject])
        case ConfigValueType.LIST    =>
          loopList(path, value.asInstanceOf[ConfigList].asScala.toList)
        case ConfigValueType.BOOLEAN =>
          Map(path -> value.toString)
        case ConfigValueType.NUMBER  =>
          loopNumber(path, value.unwrapped().asInstanceOf[Number])
        case ConfigValueType.NULL    =>
          loopNull
        case ConfigValueType.STRING  =>
          loopString(path, value.unwrapped().asInstanceOf[String])
      }

    Try(loopConfig(Chunk.empty, input.root())) match {
      case Failure(t)     =>
        throw new RuntimeException(
          "Unable to form the zio.config.PropertyTree from Hocon string." +
            " This may be due to the presence of explicit usage of nulls in hocon string. " +
            t.getMessage
        )
      case Success(value) => value
    }
  }

}
