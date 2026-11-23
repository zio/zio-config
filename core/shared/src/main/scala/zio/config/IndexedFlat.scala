package zio.config

import zio._

/**
 * A `ConfigProvider.Flat` that addresses values by `IndexedFlat.ConfigPath`, where a list index is a component of its
 * own (`KeyComponent.Index`) instead of text inside a key. An implementation provides `enumerateChildrenIndexed` and
 * `loadIndexed`. The `Chunk[String]` methods of `ConfigProvider.Flat` are final here: they convert their path with
 * `ConfigPath.fromPath`, which turns a segment such as `employees[0]` into a key name and an index, and call those two.
 *
 * {{{
 * import zio._
 * import zio.config._
 * import zio.config.IndexedFlat._
 *
 * val path: ConfigPath = ConfigPath.fromPath(Chunk("employees[0]", "age"))
 * // Chunk(KeyName(employees),Index(0),KeyName(age))
 *
 * val back: Chunk[String] = ConfigPath.toPath(path)
 * // Chunk(employees[0],age)
 * }}}
 */
trait IndexedFlat extends ConfigProvider.Flat {
  import IndexedFlat._

  /**
   * Returns the children of `path` in the source. `enumerateChildren` reports each child by the `value` of its last
   * component, so a list element is reported as `[0]`, `[1]` and so on.
   */
  def enumerateChildrenIndexed(path: ConfigPath)(implicit trace: Trace): IO[Config.Error, Set[ConfigPath]]

  /**
   * Loads the values at `path` and parses each with `config`. `split` has the meaning it has in
   * `ConfigProvider.Flat#load`.
   */
  def loadIndexed[A](path: ConfigPath, config: Config.Primitive[A], split: Boolean)(implicit
    trace: Trace
  ): IO[Config.Error, Chunk[A]]

  /**
   * Converts `path` with `ConfigPath.fromPath`, calls `enumerateChildrenIndexed`, and returns the `value` of each
   * child's last component.
   */
  final def enumerateChildren(path: Chunk[String])(implicit trace: Trace): IO[Config.Error, Set[String]] =
    enumerateChildrenIndexed(ConfigPath.fromPath(path)).map(_.map(_.last.value))

  /**
   * Same as `load(path, config, true)`.
   */
  final def load[A](path: Chunk[String], config: Config.Primitive[A])(implicit
    trace: Trace
  ): IO[Config.Error, Chunk[A]] =
    load(path, config, true)

  /**
   * Converts `path` with `ConfigPath.fromPath` and calls `loadIndexed`.
   */
  final override def load[A](path: Chunk[String], config: Config.Primitive[A], split: Boolean)(implicit
    trace: Trace
  ): IO[Config.Error, Chunk[A]] =
    loadIndexed(ConfigPath.fromPath(path), config, split)

  /**
   * Same as `loadIndexed(path, config, true)`.
   */
  final def loadIndexed[A](path: ConfigPath, config: Config.Primitive[A])(implicit
    trace: Trace
  ): IO[Config.Error, Chunk[A]] =
    loadIndexed(path, config, true)
}

/**
 * The path types of [[IndexedFlat]]: `ConfigPath` and `KeyComponent`.
 */
object IndexedFlat {

  /**
   * A config path in which a list index is a component of its own: the key `employees[0].age` is `KeyName(employees)`,
   * `Index(0)`, `KeyName(age)`.
   */
  type ConfigPath = Chunk[KeyComponent]

  /**
   * Conversions between a `ConfigPath` and the `Chunk[String]` paths of `ConfigProvider.Flat`.
   */
  object ConfigPath {

    /**
     * Converts each segment with `fromString` and joins the results: `Chunk("employees[0]", "age")` becomes
     * `KeyName(employees)`, `Index(0)`, `KeyName(age)`.
     */
    def fromPath(path: Chunk[String]): ConfigPath =
      path.flatMap(fromString)

    /**
     * Converts back to a `Chunk[String]` path, writing each index into the key name before it: `KeyName(employees)`,
     * `Index(0)`, `KeyName(age)` becomes `Chunk(employees[0], age)`.
     *
     * The conversion stops at an index that doesn't directly follow a key name, and drops that index and everything
     * after it. So `KeyName(m)`, `Index(0)`, `Index(1)`, `KeyName(x)` becomes `Chunk(m[0])`, and a path that starts
     * with an index becomes empty.
     */
    def toPath(configPath: ConfigPath): Chunk[String] = {

      def loop(configPath: List[KeyComponent], path: List[String]): Chunk[String] =
        configPath match {
          case KeyComponent.KeyName(value) :: KeyComponent.Index(index) :: tail =>
            loop(tail, s"$value[$index]" :: path)
          case KeyComponent.KeyName(value) :: tail                              =>
            loop(tail, value :: path)
          case _                                                                =>
            Chunk.fromIterable(path.reverse)
        }

      loop(configPath.toList, Nil)
    }

    private lazy val strIndexRegex = """(^.+)(\[(\d+)\])$""".stripMargin.r

    /**
     * Parses one path segment. A segment that ends in `[n]`, with at least one character before the bracket and `n` a
     * run of digits that fits in an `Int`, becomes a key name and an index: `employees[0]` gives `KeyName(employees)`,
     * `Index(0)`. Only the last bracket is read, so `a[1][2]` gives `KeyName(a[1])`, `Index(2)`. Any other segment,
     * such as `employees`, `[0]`, `a[-1]` or `a[b]`, becomes a single `KeyName` that holds the whole text.
     */
    def fromString(string: String): ConfigPath =
      strIndexRegex
        .findPrefixMatchOf(string)
        .filter(_.group(0).nonEmpty)
        .flatMap { regexMatched =>
          val optionalString: Option[String] = Option(regexMatched.group(1))
            .flatMap(s => if (s.isEmpty) None else Some(s))

          val optionalIndex: Option[Int] = Option(regexMatched.group(3))
            .flatMap(s => if (s.isEmpty) None else scala.util.Try(s.toInt).toOption)

          optionalString.flatMap(str => optionalIndex.map(ind => (str, ind)))
        }
        .fold[ConfigPath](Chunk(KeyComponent.KeyName(string))) { case (str, ind) =>
          Chunk(KeyComponent.KeyName(str), KeyComponent.Index(ind))
        }

    /**
     * Applies `patch` to `path`: converts it with `toPath`, applies the patch, and converts the result back with
     * `fromPath`. Fails when the patch fails, for example when `unnested` doesn't find its name at the start of the
     * path.
     */
    def patch(patch: ConfigProvider.Flat.PathPatch)(path: ConfigPath): Either[Config.Error, ConfigPath] =
      patch(toPath(path)).map(fromPath)
  }

  /**
   * One component of a `ConfigPath`: a key name or a list index.
   */
  sealed trait KeyComponent { self =>

    /**
     * Returns a `KeyName` with `f` applied to its name, and an `Index` unchanged.
     */
    def mapName(f: String => String): KeyComponent =
      self match {
        case KeyComponent.KeyName(name) => KeyComponent.KeyName(f(name))
        case KeyComponent.Index(value)  => KeyComponent.Index(value)
      }

    /**
     * Returns the name of a `KeyName`, or the index in square brackets for an `Index`: `Index(3).value` is `[3]`.
     */
    def value: String =
      self match {
        case KeyComponent.KeyName(name) => name
        case KeyComponent.Index(value)  => s"[$value]"
      }
  }

  /**
   * The two kinds of `KeyComponent`.
   */
  object KeyComponent {

    /**
     * A list index in a path.
     */
    final case class Index(index: Int) extends KeyComponent

    /**
     * A key name in a path.
     */
    final case class KeyName(name: String) extends KeyComponent

  }
}
