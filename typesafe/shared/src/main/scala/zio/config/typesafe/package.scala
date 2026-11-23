package zio.config

import zio._

import java.io.File

package object typesafe {

  /**
   * Adds the constructors of `TypesafeConfigProvider` to the `ConfigProvider` companion, so that
   * `import zio.config.typesafe._` makes `ConfigProvider.fromHoconString`, `ConfigProvider.fromHoconFile` and the
   * others available. Each method calls the `TypesafeConfigProvider` method of the same name.
   */
  implicit class FromConfigSourceTypesafe(configProvider: ConfigProvider.type) {

    /**
     * Builds a `ConfigProvider` from `ConfigFactory.load()`, the standard Typesafe Config lookup: it reads
     * `application.conf` from the classpath, and JVM system properties such as `-Dapp.port=7000` override its keys.
     */
    def fromResourcePath(enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromResourcePath(enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from a HOCON or JSON file, resolving its substitutions. An `include` with a relative
     * name is read relative to the file's folder. A missing file reads as an empty configuration, so a required key is
     * reported as missing data; a malformed file makes it throw a `com.typesafe.config.ConfigException.Parse`.
     */
    def fromHoconFile[A](file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconFile(file, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from the HOCON or JSON file at `filePath`, the same way as `fromHoconFile`. A missing
     * file reads as an empty configuration.
     */
    def fromHoconFilePath[A](filePath: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconFilePath(filePath, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from a HOCON or JSON string. Substitutions are resolved first: `\${path}` refers to
     * another key, and `\${?NAME}` also looks up the environment variable `NAME` and is skipped when nothing is found.
     * Throws a `com.typesafe.config.ConfigException` when the string is not valid HOCON or a required substitution
     * can't be resolved.
     *
     * {{{
     * import zio._
     * import zio.config.typesafe._
     *
     * val ports: Config[List[Int]] = Config.listOf("ports", Config.int).nested("http")
     *
     * val loaded: IO[Config.Error, List[Int]] =
     *   ConfigProvider.fromHoconString("""{ "http": { "ports": [8080, 8443] } }""").load(ports)
     * // List(8080, 8443)
     * }}}
     */
    def fromHoconString(input: String, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      TypesafeConfigProvider.fromHoconString(input, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from a `com.typesafe.config.Config` that you have already built, for example by merging
     * several files with `withFallback`. It does not resolve substitutions, so pass a resolved config: with an
     * unresolved `\${...}` it throws a `RuntimeException` whose message starts with
     * `Unable to form the zio.config.PropertyTree from Hocon string.`
     */
    def fromTypesafeConfig(
      rawConfig: com.typesafe.config.Config,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      TypesafeConfigProvider.fromTypesafeConfig(rawConfig, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromResourcePath`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromResourcePathZIO(enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TypesafeConfigProvider.fromResourcePathZIO(enableCommaSeparatedValueAsList)

    /**
     * Same as `fromHoconFile`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromHoconFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TypesafeConfigProvider.fromHoconFileZIO(file, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromHoconFilePath`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromHoconFilePathZIO(filePath: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TypesafeConfigProvider.fromHoconFilePathZIO(filePath, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromHoconString`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromHoconStringZIO(input: String, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      TypesafeConfigProvider.fromHoconStringZIO(input, enableCommaSeparatedValueAsList)

    /**
     * Same as `fromTypesafeConfig`, but returns a `Task` that fails with the exception instead of throwing it.
     */
    def fromTypesafeConfigZIO(
      rawConfig: com.typesafe.config.Config,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      TypesafeConfigProvider.fromTypesafeConfigZIO(rawConfig, enableCommaSeparatedValueAsList)
  }

}
