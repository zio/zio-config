package zio.config.yaml

import com.github.ghik.silencer.silent
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.config._

import java.io.{File, FileInputStream}
import java.lang.{Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong}
import java.nio.file.Path
import java.{util => ju}
import scala.jdk.CollectionConverters._
import scala.util.Try

@silent("Unused import")
object YamlConfigSource {
  import scala.collection.compat._
  import VersionSpecificSupport._

  private[yaml] def convertYaml(data: AnyRef): PropertyTree[String, String] =
    data match {
      case null            => PropertyTree.empty
      case t: JInteger     => PropertyTree.Leaf(t.toString)
      case t: JLong        => PropertyTree.Leaf(t.toString)
      case t: JFloat       => PropertyTree.Leaf(t.toString)
      case t: JDouble      => PropertyTree.Leaf(t.toString)
      case t: String       => PropertyTree.Leaf(t)
      case t: JBoolean     => PropertyTree.Leaf(t.toString)
      case t: ju.List[_]   =>
        PropertyTree.Sequence(
          t.asInstanceOf[ju.List[AnyRef]].asScala.toList.map(convertYaml)
        )
      case t: ju.Map[_, _] =>
        PropertyTree.Record(
          t.asInstanceOf[ju.Map[String, AnyRef]].asScala.view.mapValues(convertYaml).toMap
        )
      case _               => throw new IllegalArgumentException("unexpected data type in convertYaml")
    }

  /**
   * Retrieve a `ConfigSource` from yaml path.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[ReadError[String], MyConfig] =
   *     YamlConfigSource.fromYamlPath(Path.of("/path/to/file.yaml"))
   *       .flatMap(source => read(descriptor[MyConfig] from source)))
   * }}}
   */
  def fromYamlPath(path: Path): Either[ReadError[String], ConfigSource] =
    fromYamlFile(path.toFile)

  /**
   * Retrieve a `ConfigSource` from yaml path.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[ReadError[String], MyConfig] =
   *     YamlConfigSource.fromYamlFile(new File("/path/to/file.yaml"))
   *       .flatMap(source => read(descriptor[MyConfig] from source)))
   * }}}
   */
  def fromYamlFile(file: File): Either[ReadError[String], ConfigSource] =
    Try {
      ConfigSource.fromPropertyTree(
        convertYaml(
          new Load(LoadSettings.builder().build())
            .loadFromInputStream(new FileInputStream(file))
        ),
        file.getAbsolutePath,
        LeafForSequence.Invalid
      )
    }.toEither.swap
      .map(throwable =>
        ReadError.SourceError(
          message = s"Failed to retrieve a valid from the yaml source. ${throwable.getMessage}"
        ): ReadError[String]
      )
      .swap

  /**
   * Retrieve a `ConfigSource` from yaml path.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   val yamlString = ???
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[ReadError[String], MyConfig] =
   *     YamlConfigSource.fromYamlString(yamlString))
   *       .flatMap(source => read(descriptor[MyConfig] from source)))
   * }}}
   */
  def fromYamlString(
    yamlString: String,
    sourceName: String = "yaml"
  ): Either[ReadError[String], ConfigSource] =
    Try {
      ConfigSource.fromPropertyTree(
        convertYaml(
          new Load(LoadSettings.builder().build()).loadFromString(yamlString)
        ),
        sourceName,
        LeafForSequence.Invalid
      )
    }.toEither.swap
      .map(throwable =>
        ReadError.SourceError(
          message = s"Failed to retrieve a valid from the yaml source. ${throwable.getMessage}"
        ): ReadError[String]
      )
      .swap
}
