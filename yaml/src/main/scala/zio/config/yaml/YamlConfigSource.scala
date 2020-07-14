package zio.config.yaml

import java.io.{ File, FileInputStream }
import java.nio.file.Path
import java.lang.{ Boolean => JBoolean, Float => JFloat, Integer => JInteger }
import java.{ util => ju }

import org.snakeyaml.engine.v2.api.{ Load, LoadSettings }
import zio.blocking.{ Blocking, effectBlockingInterrupt }
import zio.config.{ ConfigSource, LeafForSequence, PropertyTree }
import zio.{ RIO, Task }

import scala.collection.JavaConverters._

object YamlConfigSource {
  private[yaml] def convertYaml(data: AnyRef): PropertyTree[String, String] =
    data match {
      case null        => PropertyTree.empty
      case t: JInteger => PropertyTree.Leaf(t.toString)
      case t: JFloat   => PropertyTree.Leaf(t.toString)
      case t: String   => PropertyTree.Leaf(t)
      case t: JBoolean => PropertyTree.Leaf(t.toString)
      case t: ju.List[_] =>
        PropertyTree.Sequence(t.asInstanceOf[ju.List[AnyRef]].asScala.toList.map(convertYaml))
      case t: ju.Map[_, _] =>
        PropertyTree.Record(
          t.asInstanceOf[ju.Map[String, AnyRef]].asScala.mapValues(convertYaml).toMap
        )
    }

  def fromPath(path: Path): RIO[Blocking, ConfigSource] =
    fromFile(path.toFile)

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
