package zio.config.yaml

import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.config._
import zio.config.syntax.IndexKey
import zio.{Chunk, ConfigProvider, Task, ZIO}

import java.io.{BufferedReader, ByteArrayInputStream, File, FileInputStream, InputStreamReader, Reader}
import java.lang.{Boolean => JBoolean, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong}
import java.nio.charset.Charset
import java.nio.file.Path
import java.{util => ju}
import scala.jdk.CollectionConverters._

/**
 * Builds a `ConfigProvider` from YAML, parsed with snakeyaml-engine. A mapping becomes nested keys and a sequence
 * becomes a list, so a case class reads from a mapping and `Config.listOf` reads from a sequence. A mapping key that
 * YAML reads as a number or a boolean, such as `8080:` or `true:`, makes every load from the provider die with a
 * `java.lang.ClassCastException`. A key with no value or with `null` is read as missing, and an empty sequence `[]` as
 * an empty list. A value written as `\${NAME}`, `\${NAME:-default}` or `\${NAME:?message}` is replaced with the
 * environment variable `NAME` while the YAML is parsed, as [[EnvConfigImpl]] describes. This applies when the
 * expression is the whole value, unquoted, and the default or message contains only letters, digits and `_`.
 *
 * Each constructor parses its input when it is called, and throws when the input can't be read or parsed; the `...ZIO`
 * variants return a `Task` that fails instead. An integer outside the `Long` range, such as `12345678901234567890`,
 * makes the constructor throw a `java.lang.RuntimeException` with the message `Invalid yaml. 12345678901234567890`. A
 * sequence nested directly in another sequence, written as `- - 1` in block style, can't be read back: loading it as a
 * list of lists fails with missing data.
 *
 * `import zio.config.yaml._` adds the same constructors to `ConfigProvider`, such as `ConfigProvider.fromYamlString`.
 *
 * {{{
 * import zio._
 * import zio.config._
 *
 * final case class Database(url: String, port: Int)
 *
 * val database: Config[Database] =
 *   Config.string("url").zip(Config.int("port")).to[Database].nested("database")
 *
 * val provider: ConfigProvider =
 *   YamlConfigProvider.fromYamlString(
 *     """|database:
 *        |  url: jdbc:postgresql://localhost/orders
 *        |  port: 5432
 *        |""".stripMargin
 *   )
 *
 * val loaded: IO[Config.Error, Database] = provider.load(database)
 * // succeeds with Database(jdbc:postgresql://localhost/orders,5432)
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-config/read-from-various-sources/ Config Providers]]
 */
object YamlConfigProvider {

  /**
   * Reads and parses a YAML file into a `ConfigProvider`. Throws a `java.io.FileNotFoundException` when the file does
   * not exist, and the snakeyaml-engine exception when the YAML is malformed; [[fromYamlFileZIO]] returns a failed
   * `Task` instead.
   *
   * {{{
   * import zio._
   * import java.io.File
   *
   * val port: IO[Config.Error, Int] =
   *   YamlConfigProvider.fromYamlFile(new File("config/application.yaml")).load(Config.int("port"))
   * }}}
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    getIndexedConfigProvider(loadYaml(file), enableCommaSeparatedValueAsList)

  /**
   * Reads and parses the YAML file at `path` into a `ConfigProvider`, like [[fromYamlFile]]. Throws a
   * `java.io.FileNotFoundException` when the file does not exist; [[fromYamlPathZIO]] returns a failed `Task` instead.
   *
   * {{{
   * import zio._
   * import java.nio.file.Paths
   *
   * val port: IO[Config.Error, Int] =
   *   YamlConfigProvider.fromYamlPath(Paths.get("config", "application.yaml")).load(Config.int("port"))
   * }}}
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromYamlFile(path.toFile, enableCommaSeparatedValueAsList)

  /**
   * Reads YAML from `reader` to the end and parses it into a `ConfigProvider`. It does not close the reader, so the
   * caller closes it once the provider is built, for example with `ZIO.acquireReleaseWith`:
   *
   * {{{
   * import zio._
   * import zio.config._
   * import java.io.InputStreamReader
   *
   * final case class Server(host: String, port: Int)
   *
   * val server: Config[Server] = Config.string("host").zip(Config.int("port")).to[Server]
   *
   * val loaded: Task[Server] =
   *   ZIO.acquireReleaseWith(
   *     ZIO.attempt(new InputStreamReader(getClass.getResourceAsStream("/application.yaml"), "UTF-8"))
   *   )(reader => ZIO.succeed(reader.close())) { reader =>
   *     YamlConfigProvider.fromYamlReaderZIO(reader).flatMap(_.load(server))
   *   }
   * }}}
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlReader(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider =
    getIndexedConfigProvider(loadYaml(reader), enableCommaSeparatedValueAsList)

  /**
   * Parses a YAML string into a `ConfigProvider`. Throws the snakeyaml-engine exception when the YAML is malformed or
   * holds more than one document, and a `java.lang.Exception` whose message is `message` when a `\${NAME:?message}`
   * variable is not set; [[fromYamlStringZIO]] returns a failed `Task` instead.
   *
   * {{{
   * import zio._
   * import zio.config._
   *
   * val hosts: IO[Config.Error, List[String]] =
   *   YamlConfigProvider
   *     .fromYamlString("hosts: eu-1.internal,eu-2.internal", enableCommaSeparatedValueAsList = true)
   *     .load(Config.listOf("hosts", Config.string))
   * // succeeds with List(eu-1.internal, eu-2.internal)
   *
   * val port: IO[Config.Error, Int] =
   *   YamlConfigProvider.fromYamlString("port: \${APP_PORT:-8080}").load(Config.int("port"))
   * // succeeds with 8080 when APP_PORT is not set
   * }}}
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlString(
    yamlString: String,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider = {
    val configStream = new ByteArrayInputStream(yamlString.getBytes(Charset.forName("UTF-8")))
    fromYamlReader(new BufferedReader(new InputStreamReader(configStream)), enableCommaSeparatedValueAsList)
  }

  /**
   * Same as [[fromYamlFile]], but suspended in a `Task`: a missing file or malformed YAML fails the task instead of
   * throwing.
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromYamlFile(file, enableCommaSeparatedValueAsList))

  /**
   * Same as [[fromYamlPath]], but suspended in a `Task`: a missing file or malformed YAML fails the task instead of
   * throwing.
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromYamlPath(path, enableCommaSeparatedValueAsList))

  /**
   * Same as [[fromYamlReader]], but suspended in a `Task`: malformed YAML fails the task instead of throwing. It does
   * not close the reader.
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlReaderZIO(
    reader: Reader,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromYamlReader(reader, enableCommaSeparatedValueAsList))

  /**
   * Same as [[fromYamlString]], but suspended in a `Task`: malformed YAML, or a `\${NAME:?message}` variable that is
   * not set, fails the task instead of throwing.
   *
   * @param enableCommaSeparatedValueAsList
   *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)` with
   *   `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
   */
  def fromYamlStringZIO(
    yamlString: String,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromYamlString(yamlString, enableCommaSeparatedValueAsList))

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
  private def loadYaml(yamlFile: File): AnyRef =
    snakeYamlLoader().loadFromInputStream(new FileInputStream(yamlFile))

  private def loadYaml(yamlReader: Reader): AnyRef =
    snakeYamlLoader().loadFromReader(yamlReader)

  private def snakeYamlLoader(): Load =
    new Load(
      LoadSettings.builder().setEnvConfig(ju.Optional.of(EnvConfigImpl)).build()
    )

}
