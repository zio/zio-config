package zio.config.typesafe

import com.github.ghik.silencer.silent
import com.typesafe.config.ConfigValueType._
import com.typesafe.config._
import zio.config.IndexedFlat.{ConfigPath, KeyComponent}
import zio.config._
import zio.{Chunk, ConfigProvider}

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
   *    TypesafeConfigSource.fromResourcePath.load(deriveConfig[MyConfig])
   * }}}
   */
  def fromResourcePath: ConfigProvider =
    fromTypesafeConfig(ConfigFactory.load.resolve)

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a given config file
   */
  def fromHoconFile[A](file: File): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve)

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a path to a config file
   */
  def fromHoconFilePath[A](filePath: String): ConfigProvider =
    fromHoconFile(new File(filePath))

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` HOCON string.
   */

  def fromHoconString(input: String): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseString(input).resolve)

  def fromTypesafeConfig(config: com.typesafe.config.Config): ConfigProvider = {
    def loop(config: com.typesafe.config.Config): Chunk[(Chunk[KeyComponent], String)] = {
      val initLevel = config.entrySet.asScala.map(entry => (entry.getKey(), entry.getValue()))

      Chunk.fromIterable(initLevel).flatMap({ case (k, possibleConfigValue) =>
        val kIterated = Chunk.fromIterable(k.split('.')).map(KeyComponent.KeyName(_))

        possibleConfigValue.valueType() match {
          case LIST    =>
            Try(config.getConfigList(k)) match {
              case Failure(_)     =>
                // Only possibility is a sequence of primitives
                val result = config.getList(k).unwrapped().asScala.toList

                Map(
                  kIterated ++ Chunk(KeyComponent.Index(0)) -> result.map(_.toString).mkString(",")
                )

              // Only possibility is a sequence of nested Configs
              case Success(value) =>
                value.asScala.toList.zipWithIndex.map { case (config: com.typesafe.config.Config, index: Int) =>
                  val oldKeyWithIndex: Chunk[KeyComponent] = kIterated ++ Chunk(KeyComponent.Index(index))

                  loop(config).map { case (newKey, v) =>
                    oldKeyWithIndex ++ newKey -> v
                  }

                }.reduceOption(_ ++ _).getOrElse(Map.empty[Chunk[KeyComponent], String])

            }
          case NUMBER  =>
            Map(kIterated -> config.getNumber(k).toString())
          case STRING  => Map(kIterated -> config.getString(k))
          case OBJECT  => throw new Exception("Invalid hocon format") //FIXME: Move to IO
          case BOOLEAN => Map(kIterated -> config.getBoolean(k).toString())
          case NULL    => throw new Exception("Invalid hocon format") // FIXME: Move to IO
        }
      })
    }

    ConfigProvider.fromIndexedMap(loop(config).map({ case (key, value) =>
      ConfigPath.toPath(key).mkString(".") -> value
    }).toMap)
  }

}
