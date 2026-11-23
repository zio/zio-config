package zio.config.typesafe

import com.typesafe.config._
import zio.config.IndexedFlat.{ConfigPath, KeyComponent}
import zio.{Chunk, ConfigProvider, Task, ZIO}

import java.io.File
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * Builds a `ConfigProvider` from HOCON, the format of the Typesafe Config library. HOCON is a superset of JSON, so the
 * same constructors read JSON. `import zio.config.typesafe._` adds these constructors to `ConfigProvider` as well, as
 * `ConfigProvider.fromHoconString` and so on.
 *
 * The constructors that take a string or a file parse it when they are called, and throw when it is not valid HOCON.
 * Each constructor has a `...ZIO` variant that returns a `Task[ConfigProvider]` and fails the task instead. A HOCON
 * object becomes nested keys, an array becomes a list, and an empty array is an empty list. An explicit `null` reads as
 * an empty string, so `Config.string("region").optional` gives `Some("")` for `region = null`, and
 * `Config.int("port").optional` fails for `port = null`. An array placed directly inside another array keeps only its
 * last element: `a = [ [1, 2], [3, 4] ]` read with `Config.listOf("a", Config.listOf(Config.int))` gives
 * `List(List(2), List(4))`.
 *
 * By default a string value is never split. With `enableCommaSeparatedValueAsList = true`, a string value read as a
 * list is split at each `,`, so `ports = "8080,8443"` read with `Config.listOf("ports", Config.int)` gives
 * `List(8080, 8443)`. HOCON arrays are lists with or without the flag.
 *
 * {{{
 * import zio._
 *
 * final case class Database(url: String, poolSize: Int)
 *
 * val database: Config[Database] =
 *   (Config.string("url") zip Config.int("poolSize")).nested("database").map { case (url, poolSize) =>
 *     Database(url, poolSize)
 *   }
 *
 * val loaded: IO[Config.Error, Database] =
 *   TypesafeConfigProvider
 *     .fromHoconString("""database { url = "postgres://localhost/orders", poolSize = 10 }""")
 *     .load(database)
 * // Database(postgres://localhost/orders,10)
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-config/read-from-various-sources/ Config providers]]
 */
object TypesafeConfigProvider {

  /**
   * Builds a `ConfigProvider` from `ConfigFactory.load()`, the standard Typesafe Config lookup: it reads
   * `application.conf` from the classpath, and JVM system properties such as `-Dapp.port=7000` override its keys.
   *
   * With this `application.conf` on the classpath:
   *
   * ```hocon
   * app {
   *   port = 8080
   *   url = "http://localhost"
   * }
   * ```
   *
   * the provider reads the `app` object:
   *
   * {{{
   * import zio._
   *
   * final case class Server(port: Int, url: String)
   *
   * val server: Config[Server] =
   *   (Config.int("port") zip Config.string("url")).nested("app").map { case (port, url) => Server(port, url) }
   *
   * val loaded: IO[Config.Error, Server] =
   *   TypesafeConfigProvider.fromResourcePath().load(server)
   * // Server(8080,http://localhost)
   * }}}
   */
  def fromResourcePath(enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.load.resolve, enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from a HOCON or JSON file, resolving its substitutions. An `include` with a relative name
   * is read relative to the file's folder. A missing file reads as an empty configuration, so a required key is
   * reported as missing data; a malformed file makes it throw a `com.typesafe.config.ConfigException.Parse`.
   */
  def fromHoconFile[A](file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseFile(file).resolve, enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from the HOCON or JSON file at `filePath`, the same way as `fromHoconFile`. A missing
   * file reads as an empty configuration.
   */
  def fromHoconFilePath[A](filePath: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromHoconFile(new File(filePath), enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from a HOCON or JSON string. Substitutions are resolved first: `\${path}` refers to
   * another key.
   * Throws a `com.typesafe.config.ConfigException` when the string is not valid HOCON or a required substitution can't
   * be resolved.
   *
   * {{{
   * import zio._
   *
   * val ports: Config[List[Int]] = Config.listOf("ports", Config.int).nested("http")
   *
   * val loaded: IO[Config.Error, List[Int]] =
   *   ConfigProvider.fromHoconString("""{ "http": { "ports": [8080, 8443] } }""").load(ports)
   * // List(8080, 8443)
   * }}}
   */
  def fromHoconString(input: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
    fromTypesafeConfig(ConfigFactory.parseString(input).resolve, enableCommaSeparatedValueAsList)

  /**
   * Builds a `ConfigProvider` from a `com.typesafe.config.Config` that you have already built, for example by merging
   * several files with `withFallback`. It does not resolve substitutions, so pass a resolved config: with an unresolved
   * `\${...}` it throws a `RuntimeException` whose message starts with
   * `Unable to form the zio.config.PropertyTree from Hocon string.`
   */
  def fromTypesafeConfig(
    config: com.typesafe.config.Config,
    enableCommaSeparatedValueAsList: Boolean = false
  ): ConfigProvider = {
    lazy val hiddenDelim = "\uFEFF"

    val indexedMapWithHiddenDelimiter =
      getIndexedMap(config).map { case (key, value) =>
        ConfigPath.toPath(key).mkString(hiddenDelim) -> value
      }

    ConfigProvider.fromMap(
      indexedMapWithHiddenDelimiter,
      pathDelim = hiddenDelim,
      seqDelim = if (enableCommaSeparatedValueAsList) "," else hiddenDelim
    )
  }

  /**
   * Same as `fromResourcePath`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromResourcePathZIO(enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromResourcePath(enableCommaSeparatedValueAsList))

  /**
   * Same as `fromHoconFile`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromHoconFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromHoconFile(file, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromHoconFilePath`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromHoconFilePathZIO(filePath: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromHoconFilePath(filePath, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromHoconString`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromHoconStringZIO(input: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
    ZIO.attempt(fromHoconString(input, enableCommaSeparatedValueAsList))

  /**
   * Same as `fromTypesafeConfig`, but returns a `Task` that fails with the exception instead of throwing it.
   */
  def fromTypesafeConfigZIO(
    config: com.typesafe.config.Config,
    enableCommaSeparatedValueAsList: Boolean = false
  ): Task[ConfigProvider] =
    ZIO.attempt(fromTypesafeConfig(config, enableCommaSeparatedValueAsList))

  private[config] def getIndexedMap(
    input: com.typesafe.config.Config
  ): Map[Chunk[KeyComponent], String] = {
    def loopNumber(path: Chunk[KeyComponent], value: Number)           = Map(path -> value.toString)
    def loopBoolean(path: Chunk[KeyComponent], value: Boolean)         = Map(path -> value.toString)
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
        val newPath = path :+ KeyComponent.KeyName(key)
        val result  = loopAny(newPath, value)
        // Usage of null to represent emptiness to satisfy Flat
        if (result.isEmpty) Map(newPath -> "") else result
      }

    def loopAny(path: Chunk[KeyComponent], value: ConfigValue): Map[Chunk[KeyComponent], String] =
      value.valueType() match {
        case ConfigValueType.OBJECT  =>
          loopConfig(path, value.asInstanceOf[ConfigObject])
        case ConfigValueType.LIST    =>
          loopList(path, value.asInstanceOf[ConfigList].asScala.toList)
        case ConfigValueType.BOOLEAN =>
          loopBoolean(path, value.unwrapped().asInstanceOf[Boolean])
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
