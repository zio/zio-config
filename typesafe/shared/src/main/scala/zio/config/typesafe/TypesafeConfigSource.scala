package zio.config.typesafe

import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.ZIO
import zio.config._

import java.io.File
import java.lang.{Boolean => JBoolean}
import scala.collection.immutable.Nil
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import zio.ConfigProvider
import com.github.ghik.silencer.silent
import com.typesafe.config._
import zio.ZIO
import zio.config._

import java.io.File
import java.lang.{Boolean => JBoolean}
import scala.collection.immutable.Nil
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}
import zio.ConfigProvider, ConfigProvider._
import zio.Chunk
import scala.util.Try

import com.typesafe.config.ConfigValueType.LIST
import com.typesafe.config.ConfigValueType.NUMBER
import com.typesafe.config.ConfigValueType.STRING
import com.typesafe.config.ConfigValueType.OBJECT
import com.typesafe.config.ConfigValueType.BOOLEAN
import com.typesafe.config.ConfigValueType.NULL
import zio.config.syntax.KeyComponent

@silent("Unused import")
object TypesafeConfigSource {
  import VersionSpecificSupport._

  //FIXME: None of the sources are in ZIO now.
  // Zio-config 3.x had ConfigSource existing as pure values, yet describing the fact
  // the load will be under the effect of ZIO
  /**
   * Retrieve a `ConfigSource` from `typesafe-config` from a given file in resource classpath.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: IO[Config.Error, MyConfig] =
   *     read(descriptor[MyConfig] from TypesafeConfigSource.fromResourcePath))
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

  def fromHoconString(input: String): syntax.ConfigProvider0 =
    fromTypesafeConfig(ConfigFactory.parseString(input).resolve)

  /**
   * Retrieve a `ConfigSource` from `typesafe-config` data type.
   *
   * With zio.config 3.x, https://github.com/lightbend/config/issues/30 was resolved within zio-config.
   * However this feature is removed with zio.Config 4.0.0
   */
  def fromTypesafeConfig(config: com.typesafe.config.Config): syntax.ConfigProvider0 = {
    def loop(config: com.typesafe.config.Config): Map[Chunk[syntax.KeyComponent], String] = {
      val initLevel = config.entrySet.asScala.map(entry => (entry.getKey(), entry.getValue())).toMap

      initLevel.flatMap({ case (k, possibleConfigValue) =>
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
          case OBJECT  => throw new Exception("It shouldn't happen")      //FIXME: Move to IO
          case BOOLEAN => Map(kIterated -> config.getBoolean(k).toString())
          case NULL    => throw new Exception("Well I can't do anything") // FIXME: Move to IO
        }
      })
    }

    ConfigProvider.fromIndexedFlat(syntax.IndexedFlat.from(loop(config)))
  }

}
