package zio.config.yaml

import com.github.ghik.silencer.silent
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.config._
import zio.{ZIO, ZManaged}

import java.io.{File, FileInputStream, Reader}
import java.lang.{Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong}
import java.nio.file.Path
import java.{util => ju}
import scala.jdk.CollectionConverters._

import ConfigSource.{Reader => ConfigSourceReader, _}

@silent("Unused import")
object YamlConfigSource {
  import scala.collection.compat._
  import VersionSpecificSupport._

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
  def fromYamlFile(file: File): ConfigSource =
    fromYamlRepr(file)(loadYaml(_), file.getAbsolutePath)

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
  def fromYamlPath(path: Path): ConfigSource =
    fromYamlFile(path.toFile)

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
  ): ConfigSource =
    fromYamlRepr(reader)(loadYaml(_), sourceName)

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
  ): ConfigSource =
    fromYamlRepr(yamlString)(loadYaml(_), sourceName)

  private[config] def fromYamlRepr[A](repr: A)(
    loadYaml: A => ZIO[Any, ReadError[String], AnyRef],
    sourceName: String = "yaml"
  ): ConfigSource = {

    val managedTree =
      loadYaml(repr).flatMap(anyRef => convertYaml(anyRef)).toManaged_

    ConfigSourceReader(
      Set(ConfigSourceName(sourceName)),
      ZManaged.succeed(managedTree.map(tree => (path: PropertyTreePath[String]) => ZIO.succeed(tree.at(path))))
    ).memoize
  }

  private[yaml] def convertYaml(data: AnyRef): ZIO[Any, ReadError[String], PropertyTree[String, String]] = {
    def strictLeaf[A](leaf: A) = PropertyTree.Leaf(leaf, canBeSequence = false)

    data match {
      case null          => ZIO.succeed(PropertyTree.empty)
      case t: JInteger   => ZIO.succeed(strictLeaf(t.toString))
      case t: JLong      => ZIO.succeed(strictLeaf(t.toString))
      case t: JFloat     => ZIO.succeed(strictLeaf(t.toString))
      case t: JDouble    => ZIO.succeed(strictLeaf(t.toString))
      case t: String     => ZIO.succeed(strictLeaf(t))
      case t: JBoolean   => ZIO.succeed(strictLeaf(t.toString))
      case t: ju.List[_] =>
        ZIO
          .foreach(
            t.asInstanceOf[ju.List[AnyRef]].asScala.toList
          )(each => convertYaml(each))
          .map(PropertyTree.Sequence(_))

      case t: ju.Map[_, _] =>
        ZIO
          .foreach(
            t.asInstanceOf[ju.Map[String, AnyRef]].asScala.toList
          )(each => convertYaml(each._2).map(r => (each._1, r)))
          .map(res => PropertyTree.Record(res.toMap))

      case _ => ZIO.fail(ReadError.SourceError("unexpected data type in convertYaml"))
    }
  }

  private def loadYaml(yamlFile: File): ZIO[Any, ReadError[String], AnyRef] =
    snakeYamlLoader().flatMap(r =>
      ZIO
        .effect(r.loadFromInputStream(new FileInputStream(yamlFile)))
        .mapError(throwable => ReadError.SourceError(throwable.toString))
    )

  private def loadYaml(yamlReader: Reader): ZIO[Any, ReadError[String], AnyRef] =
    snakeYamlLoader().flatMap(r =>
      ZIO
        .effect(r.loadFromReader(yamlReader))
        .mapError(throwable => ReadError.SourceError(throwable.toString))
    )

  private def loadYaml(yamlString: String): ZIO[Any, ReadError[String], AnyRef] =
    snakeYamlLoader().flatMap(r =>
      ZIO
        .effect(r.loadFromString(yamlString))
        .mapError(throwable => ReadError.SourceError(throwable.toString))
    )

  private def snakeYamlLoader(): ZIO[Any, ReadError[String], Load] =
    ZIO
      .effect(
        new Load(
          LoadSettings.builder().setEnvConfig(ju.Optional.of(EnvConfigImpl)).build()
        ): Load
      )
      .mapError(throwable =>
        ReadError.SourceError(s"Failed to load snake yaml environment config ${throwable.toString}")
      )
}
