package zio.config.yaml

import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.config._
import zio.config.syntax.IndexKey
import zio.{Cause, Chunk, Config, ConfigProvider, IO, Task, Trace, ZIO}

import java.io.{BufferedReader, ByteArrayInputStream, File, FileInputStream, InputStreamReader, Reader}
import java.lang.{Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong}
import java.nio.charset.Charset
import java.nio.file.Path
import java.{util => ju}
import scala.jdk.CollectionConverters._

object YamlConfigProvider {

  /**
   * Retrieve a `ConfigSource` from yaml path.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[Config.Error, MyConfig] =
   *     ConfigProvider.fromYamlFile(new File("/path/to/file.yaml"))
   *       .flatMap(source => read(deriveConfig[MyConfig] from source)))
   * }}}
   */
  def fromYamlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromLazyProvider(getIndexedConfigProvider(loadYaml(file), enableCommaSeparatedValueAsList))

  /**
   * Retrieve a `ConfigSource` from yaml path.
   *
   * A complete example usage:
   *
   * {{{
   *
   *   case class MyConfig(port: Int, url: String)
   *
   *   val result: Either[Config.Error, MyConfig] =
   *     ConfigProvider.fromYamlPath(Path.of("/path/to/file.yaml"))
   *       .flatMap(source => read(deriveConfig[MyConfig] from source)))
   * }}}
   */
  def fromYamlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromYamlFile(path.toFile, enableCommaSeparatedValueAsList)

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
   *   val myConfig: InputStreamReader => IO[Config.Error, MyConfig] = reader =>
   *     IO.fromEither(
   *        for {
   *          source <- ConfigProvider.fromYamlReader(reader)
   *          myConfig <- read(deriveConfig[MyConfig] from source)
   *        } yield myConfig
   *     )
   *
   *   val result: Task[MyConfig] = ZManaged.make(acquire("relative/path/to/your/application/resource.yml"))(release).use(myConfig)
   * }}}
   */
  def fromYamlReader(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider =
    fromLazyProvider(getIndexedConfigProvider(loadYaml(reader), enableCommaSeparatedValueAsList))

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
   *   val result: Either[Config.Error, MyConfig] =
   *     ConfigProvider.fromYamlString(yamlString))
   *       .flatMap(source => read(deriveConfig[MyConfig] from source)))
   * }}}
   */
  def fromYamlString(
    yamlString: String,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider =
    fromLazyProvider {
      val configStream = new ByteArrayInputStream(yamlString.getBytes(Charset.forName("UTF-8")))
      getIndexedConfigProvider(
        loadYaml(new BufferedReader(new InputStreamReader(configStream))),
        enableCommaSeparatedValueAsList
      )
    }

  def fromYamlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromYamlFile(file, enableCommaSeparatedValueAsList))

  def fromYamlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromYamlPath(path, enableCommaSeparatedValueAsList))

  def fromYamlReaderZIO(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromYamlReader(reader, enableCommaSeparatedValueAsList))

  def fromYamlStringZIO(
    yamlString: String,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromYamlString(yamlString, enableCommaSeparatedValueAsList))

  private def fromLazyProvider(makeProvider: => ConfigProvider): ConfigProvider =
    ConfigProvider.fromFlat(new ConfigProvider.Flat {
      lazy val underlying: IO[Config.Error, ConfigProvider.Flat] =
        ZIO
          .attempt(makeProvider.flatten)
          .mapError(throwable =>
            Config.Error.SourceUnavailable(
              Chunk.empty,
              Option(throwable.getMessage).getOrElse(throwable.toString),
              Cause.fail(throwable)
            )
          )

      def load[A](path: Chunk[String], config: Config.Primitive[A])(implicit
        trace: Trace
      ): IO[Config.Error, Chunk[A]] =
        underlying.flatMap(_.load(path, config))

      override def load[A](path: Chunk[String], config: Config.Primitive[A], split: Boolean)(implicit
        trace: Trace
      ): IO[Config.Error, Chunk[A]] =
        underlying.flatMap(_.load(path, config, split))

      def enumerateChildren(path: Chunk[String])(implicit
        trace: Trace
      ): IO[Config.Error, Set[String]] =
        underlying.flatMap(_.enumerateChildren(path))
    })

  private[yaml] def getIndexedConfigProvider(
    data: AnyRef,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider = {
    def flattened(data: AnyRef, chunk: Chunk[String]): Map[Chunk[String], String] =
      data match {
        case null        => Map.empty
        case t: JInteger => Map(chunk -> t.toString())
        case t: JLong    => Map(chunk -> t.toString())
        case t: JFloat   => Map(chunk -> t.toString())
        case t: JDouble  => Map(chunk -> t.toString())
        case t: String   => Map(chunk -> t.toString())
        case t: JBoolean => Map(chunk -> t.toString())

        case t: java.lang.Iterable[_] =>
          val list = t.asInstanceOf[java.lang.Iterable[AnyRef]].asScala.toList

          if (list.isEmpty) {
            Map(chunk -> "<nil>")
          } else {
            list.zipWithIndex.map { case (anyRef, index) =>
              flattened(anyRef, chunk.mapLast(last => last + IndexKey(index)))
            }
              .reduceOption(_ ++ _)
              .getOrElse(Map.empty)
          }

        case t: ju.Map[_, _] =>
          val map = t.asInstanceOf[ju.Map[String, AnyRef]].asScala.toMap

          map.map { case (k, v) =>
            flattened(v, chunk :+ k)
          }
            .reduceOption(_ ++ _)
            .getOrElse(Map.empty)

        case a =>
          throw new RuntimeException(
            s"Invalid yaml. ${a}"
          )
      }

    lazy val hiddenDelim = "\uFEFF"

    ConfigProvider.fromMap(
      flattened(data, Chunk.empty).map { case (k, v) => (k.mkString(hiddenDelim), v) },
      pathDelim = hiddenDelim,
      seqDelim = if (enableCommaSeparatedValueAsList) "," else hiddenDelim
    )

  }

  // It's hard to define a ConfigProvider on its on without effectful ConfigSource (similar to ZIO-Config 3.x)
  private def loadYaml(yamlFile: File): AnyRef = {
    val stream = new FileInputStream(yamlFile)
    try snakeYamlLoader().loadFromInputStream(stream)
    finally stream.close()
  }

  private def loadYaml(yamlReader: Reader): AnyRef =
    snakeYamlLoader().loadFromReader(yamlReader)

  private def snakeYamlLoader(): Load =
    new Load(
      LoadSettings.builder().setEnvConfig(ju.Optional.of(EnvConfigImpl)).build()
    )

}
