package zio.config

import zio._

import java.io.{File, Reader}
import java.nio.file.Path

package object yaml {

  /**
   * Adds the YAML constructors of [[YamlConfigProvider]] to the `ConfigProvider` companion, so that
   * `import zio.config.yaml._` makes `ConfigProvider.fromYamlString`, `ConfigProvider.fromYamlFile` and the others
   * available. It also adds `fromYamlRepr`, which takes YAML that is already parsed.
   *
   * @see
   *   [[https://zio.dev/zio-config/read-from-various-sources/ Config Providers]]
   */
  implicit class FromConfigSourceYaml(c: ConfigProvider.type) {

    /**
     * Reads and parses a YAML file into a `ConfigProvider`; the same as `YamlConfigProvider.fromYamlFile`. Throws when
     * the file does not exist or the YAML is malformed.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlFile(file: File, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      YamlConfigProvider.fromYamlFile(file, enableCommaSeparatedValueAsList)

    /**
     * Reads and parses the YAML file at `path` into a `ConfigProvider`; the same as `YamlConfigProvider.fromYamlPath`.
     * Throws when the file does not exist or the YAML is malformed.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlPath(path: Path, enableCommaSeparatedValueAsList: Boolean = false): ConfigProvider =
      YamlConfigProvider.fromYamlPath(path, enableCommaSeparatedValueAsList)

    /**
     * Reads YAML from `reader` to the end and parses it into a `ConfigProvider`; the same as
     * `YamlConfigProvider.fromYamlReader`. It does not close the reader.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlReader(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      YamlConfigProvider.fromYamlReader(reader, enableCommaSeparatedValueAsList)

    /**
     * Parses a YAML string into a `ConfigProvider`; the same as `YamlConfigProvider.fromYamlString`. Throws when the
     * YAML is malformed or a `\${NAME:?message}` variable is not set.
     *
     * {{{
     * import zio._
     * import zio.config._
     * import zio.config.yaml._
     *
     * val replicas: IO[Config.Error, List[String]] =
     *   ConfigProvider
     *     .fromYamlString(
     *       """|replicas:
     *          |  - eu-west-1
     *          |  - eu-central-1
     *          |""".stripMargin
     *     )
     *     .load(Config.listOf("replicas", Config.string))
     * // succeeds with List(eu-west-1, eu-central-1)
     * }}}
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlString(
      yamlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      YamlConfigProvider.fromYamlString(yamlString, enableCommaSeparatedValueAsList)

    /**
     * Builds a `ConfigProvider` from YAML that another parser has already read. `loadYaml` turns `repr` into the values
     * snakeyaml-engine produces: a `java.util.Map` with string keys, a `java.lang.Iterable`, a `String`, a boxed
     * `Integer`, `Long`, `Float`, `Double` or `Boolean`, or `null`. Any other value makes it throw a
     * `java.lang.RuntimeException` whose message starts with `Invalid yaml.`.
     *
     * {{{
     * import zio._
     * import zio.config._
     * import zio.config.yaml._
     *
     * val parsed: java.util.Map[String, AnyRef] = new java.util.HashMap[String, AnyRef]()
     * parsed.put("port", Integer.valueOf(8443))
     *
     * val port: IO[Config.Error, Int] =
     *   ConfigProvider.fromYamlRepr(parsed)(identity).load(Config.int("port"))
     * // succeeds with 8443
     * }}}
     *
     * @param repr
     *   the already parsed input
     * @param loadYaml
     *   turns `repr` into Java maps, lists and scalar values
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlRepr[A](repr: A)(
      loadYaml: A => AnyRef,
      enableCommaSeparatedValueAsList: Boolean = false
    ): ConfigProvider =
      YamlConfigProvider.getIndexedConfigProvider(loadYaml(repr), enableCommaSeparatedValueAsList)

    /**
     * Same as `ConfigProvider.fromYamlFile`, but suspended in a `Task`: a missing file or malformed YAML fails the task
     * instead of throwing.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlFileZIO(file: File, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      YamlConfigProvider.fromYamlFileZIO(file, enableCommaSeparatedValueAsList)

    /**
     * Same as `ConfigProvider.fromYamlPath`, but suspended in a `Task`: a missing file or malformed YAML fails the task
     * instead of throwing.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlPathZIO(path: Path, enableCommaSeparatedValueAsList: Boolean = false): Task[ConfigProvider] =
      YamlConfigProvider.fromYamlPathZIO(path, enableCommaSeparatedValueAsList)

    /**
     * Same as `ConfigProvider.fromYamlReader`, but suspended in a `Task`: malformed YAML fails the task instead of
     * throwing. It does not close the reader.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlReaderZIO(
      reader: Reader,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      YamlConfigProvider.fromYamlReaderZIO(reader, enableCommaSeparatedValueAsList)

    /**
     * Same as `ConfigProvider.fromYamlString`, but suspended in a `Task`: malformed YAML, or a `\${NAME:?message}`
     * variable that is not set, fails the task instead of throwing.
     *
     * @param enableCommaSeparatedValueAsList
     *   when `true`, a string value that is read as a list is split on commas, so `hosts: a,b` reads as `List(a, b)`
     *   with `Config.listOf`; `false` by default, where it reads as the one-element list `List(a,b)`
     */
    def fromYamlStringZIO(
      yamlString: String,
      enableCommaSeparatedValueAsList: Boolean = false
    ): Task[ConfigProvider] =
      YamlConfigProvider.fromYamlStringZIO(yamlString, enableCommaSeparatedValueAsList)
  }

}
