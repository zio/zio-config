package zio.config.yaml

import com.github.ghik.silencer.silent
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.config._

import java.io.{File, FileInputStream, Reader}
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
    loadFromPropertyTree(
      convertYaml(
        loadYaml(file)
      ),
      file.getAbsolutePath
    )

  /**
   * Retrieve a `ConfigSource` from yaml reader.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   import zio.*
   *   import scala.io.Source
   *   import java.io.InputStreamReader
   *   import zio.config.yaml.YamlConfigSource
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   def acquire(yamlResourcePath: String) = ZIO.effect(Source.fromResource(yamlResourcePath).reader)
   *   def release(reader: InputStreamReader) = ZIO.effectTotal(reader.close())
   *
   *   val myConfig: InputStreamReader => IO[ReadError[String], MyConfig] = reader =>
   *     IO.fromEither(
   *        for {
   *          source <- YamlConfigSource.fromYamlReader(reader)
   *          myConfig <- read(descriptor[MyConfig] from source)
   *        } yield myConfig
   *     )
   *
   *   val result: Task[MyConfig] = ZManaged.make(acquire("relative/path/to/your/application/resource.yml"))(release).use(myConfig)
   * }}}
   */
  def fromYamlReader(
    reader: Reader,
    sourceName: String = "yaml"
  ): Either[ReadError[String], ConfigSource] =
    loadFromPropertyTree(
      convertYaml(
        loadYaml(reader)
      ),
      sourceName
    )

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
    loadFromPropertyTree(
      convertYaml(
        loadYaml(yamlString)
      ),
      sourceName
    )

  private def loadFromPropertyTree(
    propertyTree: => PropertyTree[String, String],
    sourceName: String
  ): Either[ReadError[String], ConfigSource] =
    Try {
      ConfigSource.fromPropertyTree(
        propertyTree,
        sourceName,
        LeafForSequence.Invalid
      )
    }.toEither.swap
      .map(throwable =>
        ReadError.SourceError(
          message = s"Failed to retrieve a valid yaml from the yaml source. ${throwable.getMessage}",
          Set.empty
        )
      )
      .swap

  private def loadYaml(yamlFile: File): AnyRef =
    snakeYamlLoader().loadFromInputStream(new FileInputStream(yamlFile))

  private def loadYaml(yamlReader: Reader): AnyRef =
    snakeYamlLoader().loadFromReader(yamlReader)

  private def loadYaml(yamlString: String): AnyRef =
    snakeYamlLoader().loadFromString(yamlString)

  private def snakeYamlLoader(): Load =
    new Load(
      LoadSettings.builder().setEnvConfig(ju.Optional.of(EnvConfigImpl)).build()
    )
}
