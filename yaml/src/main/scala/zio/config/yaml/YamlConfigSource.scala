package zio.config.yaml

import java.io.{ File, FileInputStream }
import java.nio.file.Path
import java.lang.{ Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong }
import java.{ util => ju }

import org.snakeyaml.engine.v2.api.{ Load, LoadSettings }
import zio.blocking.{ effectBlockingInterrupt, Blocking }
import zio.config.{ ConfigSource, LeafForSequence, PropertyTree }
import zio.{ RIO, Task }

import scala.collection.JavaConverters._

object YamlConfigSource {
  private[yaml] def convertYaml(data: AnyRef): PropertyTree[String, String] =
    data match {
      case null        => PropertyTree.empty
      case t: JInteger => PropertyTree.Leaf(t.toString)
      case t: JLong    => PropertyTree.Leaf(t.toString)
      case t: JFloat   => PropertyTree.Leaf(t.toString)
      case t: JDouble  => PropertyTree.Leaf(t.toString)
      case t: String   => PropertyTree.Leaf(t)
      case t: JBoolean => PropertyTree.Leaf(t.toString)
      case t: ju.List[_] =>
        PropertyTree.Sequence(t.asInstanceOf[ju.List[AnyRef]].asScala.toList.map(convertYaml))
      case t: ju.Map[_, _] =>
        PropertyTree.Record(
          t.asInstanceOf[ju.Map[String, AnyRef]].asScala.mapValues(convertYaml).toMap
        )
    }

  /**
   * Creates a configuration source from a YAML file at the specified path.
   */
  def fromPath(path: Path): RIO[Blocking, ConfigSource] =
    fromFile(path.toFile)

  /**
   * Creates a configuration source from a YAML file.
   */
  def fromFile(file: File): RIO[Blocking, ConfigSource] =
    effectBlockingInterrupt {
      ConfigSource.fromPropertyTree(
        convertYaml(
          new Load(LoadSettings.builder().build()).loadFromInputStream(new FileInputStream(file))
        ),
        file.getAbsolutePath,
        LeafForSequence.Invalid
      )
    }

  /**
   * Creates a configuration source from a YAML string.
   */
  def fromString(yamlString: String, sourceName: String = "yaml"): Task[ConfigSource] =
    Task {
      ConfigSource.fromPropertyTree(
        convertYaml(
          new Load(LoadSettings.builder().build()).loadFromString(yamlString)
        ),
        sourceName,
        LeafForSequence.Invalid
      )
    }
}
