package zio.config

import zio.Config

/**
 * Generates documentation for a `Config`. `generateDocs` describes a config as `ConfigDocs`, `ConfigDocs#toTable` turns
 * that into a `Table`, and the table renders as Markdown. The `zio.config` package object extends this trait, so
 * `import zio.config._` brings `generateDocs` and `Table` into scope.
 *
 * {{{
 * import zio._
 * import zio.config._
 *
 * val database: Config[(String, Int)] =
 *   (Config.string("host") ?? "Database host").zip(Config.int("port") ?? "Database port").nested("database")
 *
 * val markdown: String = generateDocs(database).toTable.toGithubFlavouredMarkdown
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
 */
trait ConfigDocsModule {
  import Table._

  /**
   * The documentation of a `Config`, built by `generateDocs`: its keys, their descriptions, and how they combine
   * (nested keys, `zip`, `orElse`, lists and maps). Its cases are not public; convert it with `toTable` to read or
   * render it.
   */
  sealed trait ConfigDocs { self =>
    private def is[A](f: PartialFunction[ConfigDocs, A])(orElse: A): A =
      f.applyOrElse(self, (_: ConfigDocs) => orElse)

    /**
     * Converts these docs into a `Table`. Each row holds a key with its format and descriptions, and a row whose key
     * holds other keys points to a nested table. Render it with `Table#toGithubFlavouredMarkdown`,
     * `Table#toConfluenceMarkdown` or `Table#toMarkdown`.
     */
    def toTable: Table = {
      def filterDescriptions(
        descriptionsUsedAlready: Option[Table.FieldName],
        descriptions: List[ConfigDocs.Description]
      ) = {
        val desc =
          descriptionsUsedAlready match {
            case Some(value) =>
              descriptions.filter {
                case ConfigDocs.Description(path, _) if path.map(FieldName.Key.apply) == Some(value) =>
                  false
                case ConfigDocs.Description(_, _)                                                    => true
              }
            case None        => descriptions
          }
        desc
      }

      def go(
        docs: ConfigDocs,
        previousPaths: List[FieldName],
        previousNode: Option[ConfigDocs],
        descriptionsUsedAlready: Option[FieldName]
      ): Table = {
        def handleNested(
          left: ConfigDocs,
          right: ConfigDocs,
          currentNode: ConfigDocs,
          isNode: ConfigDocs => Boolean,
          format: Format
        ): Table =
          if (previousNode.exists(isNode)) {
            go(left, previousPaths, Some(currentNode), descriptionsUsedAlready) ++
              go(
                right,
                previousPaths,
                Some(currentNode),
                descriptionsUsedAlready
              )
          } else {
            val tableWithAllDescriptions =
              (go(
                left,
                previousPaths,
                Some(currentNode),
                descriptionsUsedAlready
              ) ++
                go(
                  right,
                  previousPaths,
                  Some(currentNode),
                  descriptionsUsedAlready
                ))

            // Look backwards and see if previous node is nested, if so remove the already used descriptions
            val parentDoc =
              if (previousNode.exists(r => r.is { case ConfigDocs.Nested(_, _, _) => true }(false))) {
                previousPaths.lastOption match {
                  case Some(value) =>
                    ConfigDocs
                      .findByPath(
                        tableWithAllDescriptions.rows.flatMap(_.description),
                        value
                      )
                      .distinct
                  case None        => Nil
                }
              } else {
                Nil
              }

            val leftSide =
              go(left, Nil, Some(currentNode), previousPaths.lastOption)

            val rightSide =
              go(right, Nil, Some(currentNode), previousPaths.lastOption)

            TableRow(
              previousPaths,
              Some(format),
              parentDoc,
              Some((leftSide ++ rightSide)),
              Set.empty
            ).asTable
          }

        docs match {

          case ConfigDocs.Leaf(descriptions, _) =>
            val desc = filterDescriptions(descriptionsUsedAlready, descriptions)

            TableRow(
              previousPaths,
              Some(Table.Format.Primitive),
              desc,
              None,
              Set.empty
            ).asTable

          case c @ ConfigDocs.Nested(path, docs, descriptions) =>
            val descs  =
              filterDescriptions(descriptionsUsedAlready, descriptions)
            val result =
              go(
                docs,
                previousPaths :+ Table.FieldName.Key(path),
                Some(c),
                descriptionsUsedAlready = descriptionsUsedAlready
              )

            // Peak ahead and see if it continues to do nesting.
            // For example: nested("a")(string) isn't treated as nested.
            // However a is nested if config is nested("a")(nested("b")(string))
            if (
              docs.is { case ConfigDocs.Nested(_, _, _) =>
                true
              }(false)
            ) {
              TableRow(
                previousPaths :+ Table.FieldName.Key(path),
                Some(Table.Format.Nested),
                descs,
                Some(result),
                Set.empty
              ).asTable
            } else
              result.copy(
                rows = result.rows
                  .map(row => row.copy(description = row.description ++ descs))
              )

          case c @ ConfigDocs.Zip(left, right) =>
            handleNested(
              left,
              right,
              c,
              _.is { case ConfigDocs.Zip(_, _) =>
                true
              }(false),
              Format.AllOf
            )

          case c @ ConfigDocs.OrElse(left, right) =>
            handleNested(
              left,
              right,
              c,
              _.is { case ConfigDocs.OrElse(_, _) =>
                true
              }(false),
              Format.AnyOneOf
            )

          case c @ ConfigDocs.Sequence(schemaDocs, _) =>
            go(schemaDocs, previousPaths, Some(c), descriptionsUsedAlready).mapFormat {
              case Some(Format.Recursion) => Format.RecursionList
              case _                      => Format.List
            }

          case c @ ConfigDocs.DynamicMap(schemaDocs, _) =>
            go(schemaDocs, previousPaths, Some(c), descriptionsUsedAlready)
              .withFormat(Format.Map)
        }
      }

      go(self, Nil, None, None)
    }
  }

  private[config] object ConfigDocs {
    sealed case class Description(path: Option[String], description: String)

    def findByPath(description: List[Description], path: FieldName): List[Description] =
      description
        .flatMap(desc =>
          desc match {
            case a @ ConfigDocs.Description(p, _) if (p.map(FieldName.Key.apply)) == Some(path) =>
              List(a)
            case ConfigDocs.Description(_, _)                                                   => Nil
          }
        )

    sealed case class Leaf(descriptions: List[Description], value: Option[String] = None)        extends ConfigDocs
    sealed case class Nested(path: String, docs: ConfigDocs, descriptions: List[Description])    extends ConfigDocs
    sealed case class Zip(left: ConfigDocs, right: ConfigDocs)                                   extends ConfigDocs
    sealed case class OrElse(leftDocs: ConfigDocs, rightDocs: ConfigDocs)                        extends ConfigDocs
    sealed case class Sequence(schemaDocs: ConfigDocs, valueDocs: List[ConfigDocs] = List.empty) extends ConfigDocs
    sealed case class DynamicMap(
      schemaDocs: ConfigDocs,
      valueDocs: Map[String, ConfigDocs] = Map.empty[String, ConfigDocs]
    ) extends ConfigDocs

  }

  /**
   * A table of config documentation, built by `ConfigDocs#toTable`. Each `Table.TableRow` describes a key, or a group
   * of keys, and can point to a nested table for the keys under it. Render it with `toGithubFlavouredMarkdown`,
   * `toConfluenceMarkdown` or `toMarkdown`, or read `rows` to produce another format.
   *
   * @param rows
   *   the rows of this table, in the order of the config
   */
  sealed case class Table(rows: List[TableRow]) { self =>

    /**
     * Returns a table with the rows of this table followed by the rows of `that`.
     */
    def ++(that: Table): Table =
      Table(rows ++ that.rows)

    /**
     * Renders this table as Markdown for Atlassian Confluence, with `Table.confluenceFlavoured` links of the form
     * `[name|target]`. With `Some(url)`, a link to the heading `database` targets `url-database`; with `None`, it
     * targets `database`.
     *
     * @param baseUrl
     *   the URL of the Confluence page that shows the Markdown, or `None`
     */
    def toConfluenceMarkdown(
      baseUrl: Option[String]
    ): String =
      toMarkdown(Table.confluenceFlavoured(baseUrl))

    /**
     * Renders this table as GitHub-flavoured Markdown: a `## Configuration Details` heading, then one Markdown table
     * for this table and one under a `###` heading for each nested table. In a row with a nested table, the format, and
     * the name when the row has a key, link to that table's heading.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val markdown: String =
     *   generateDocs(Config.int("port") ?? "HTTP port").toTable.toGithubFlavouredMarkdown
     * //
     * // ## Configuration Details
     * //
     * //
     * // |FieldName|Format   |Description                   |Sources|
     * // |---      |---      |---                           |---    |
     * // |port     |primitive|an integer property, HTTP port|       |
     * }}}
     */
    def toGithubFlavouredMarkdown: String =
      toMarkdown(Table.githubFlavoured)

    /**
     * Renders this table as Markdown, building each link with `getLink`. For a row that has a nested table, `getLink`
     * receives the heading of that table, the heading's index, and either the row's field name or its format. The index
     * is `0` for the first use of a heading and goes up by one for each further use; rows are counted from the last row
     * of a table to the first, and a nested table continues the count of the table that contains it. Rows without a
     * nested table show plain text. `toGithubFlavouredMarkdown` and `toConfluenceMarkdown` call this method with
     * `Table.githubFlavoured` and `Table.confluenceFlavoured`.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val table: Table = generateDocs(Config.int("port").zip(Config.string("host")).nested("server")).toTable
     *
     * val plain: String =
     *   table.toMarkdown((_, _, nameOrFormat) => Table.Link.rawString(nameOrFormat.fold(_.asString(None), _.asString)))
     * }}}
     */
    def toMarkdown(
      getLink: (Heading, Int, Either[FieldName, Format]) => Link
    ): String = {
      val headingColumns =
        List("FieldName", "Format", "Description", "Sources")

      def updateHeadingAndIndex(heading: Heading, map: Map[Heading, Int]): Map[Heading, Int] = {
        val index = map.get(heading).map(index => index + 1).getOrElse(0)
        map.updated(heading, index)
      }

      def convertHeadingToString(paths: List[FieldName]): String =
        paths.map(_.asString(Some("Field Descriptions"))).mkString(".")

      def go(table: Table, usedHeadings: Map[Heading, Int]): List[String] = {
        val (contents, nestedTables, updatedUsedHeadings): (
          List[List[String]],
          List[
            (Table, Heading)
          ],
          Map[Heading, Int]
        ) =
          table.rows
            .foldRight(
              (
                List.empty[List[String]],
                List.empty[(Table, Heading)],
                usedHeadings
              )
            ) { case (row, (contentList, nestedTableList, usedHeadings)) =>
              val lastFieldName =
                row.paths.lastOption.getOrElse(FieldName.Blank)

              val formatOrNotApplicable =
                row.format.getOrElse(Format.NotApplicable)
              val heading               = Heading.mk(row.paths)
              val updatedHeading        =
                updateHeadingAndIndex(heading, usedHeadings)

              val (name, format) = row.nested match {
                case Some(_) =>
                  val getLinkFn =
                    (s: Either[FieldName, Format]) =>
                      getLink(
                        heading,
                        updatedHeading.getOrElse(heading, 0),
                        s
                      )

                  val nameWithLink =
                    lastFieldName match {
                      case FieldName.Key(_) => getLinkFn(Left(lastFieldName))
                      case FieldName.Blank  =>
                        Link.rawString(lastFieldName.asString(None))
                    }

                  nameWithLink -> getLinkFn(Right(formatOrNotApplicable))

                case None =>
                  Link.rawString(lastFieldName.asString(None)) -> Link
                    .rawString(formatOrNotApplicable.asString)
              }

              (
                List(
                  name.value,
                  format.value,
                  row.description.map(_.description).mkString(", "),
                  row.sources.mkString(", ")
                ) :: contentList,
                row.nested
                  .map(table => (table, heading))
                  .toList ++ nestedTableList,
                updatedHeading
              )
            }

        val contentList: List[String] = {
          val indexAndSize = getSizeOfIndices(headingColumns :: contents)

          (headingColumns :: List.fill(indexAndSize.size)("---") :: contents)
            .map(fieldValues =>
              mkStringAndWrapWith(
                fieldValues.zipWithIndex.map { case (string, index) =>
                  padToEmpty(string, indexAndSize.getOrElse(index, 0))
                },
                "|"
              )
            )
        }

        contentList.mkString(System.lineSeparator()) ::
          nestedTables.flatMap(table =>
            mkStringAndWrapWith(
              List(s"### ${convertHeadingToString(table._2.path)}"),
              System.lineSeparator()
            ) :: go(table._1, updatedUsedHeadings)
          )
      }

      mkStringAndWrapWith(
        s"## Configuration Details" :: (System
          .lineSeparator() :: go(self, Map.empty)),
        System.lineSeparator()
      )
    }

    /**
     * Returns a table whose rows have their format replaced with `f` of the current format. Nested tables are not
     * changed.
     */
    def mapFormat(f: Option[Format] => Format): Table =
      Table(rows.map(row => row.copy(format = Some(f(row.format)))))

    /**
     * Returns a table whose rows all have the format `format`. Nested tables are not changed.
     */
    def withFormat(format: Format): Table =
      mapFormat(_ => format)

    private def padToEmpty(string: String, size: Int): String = {
      val maxSize = Math.max(string.length, size)
      string.padTo(maxSize, ' ')
    }

    private def wrapWith(input: String, str: String): String =
      str ++ input ++ str

    private def mkStringAndWrapWith(input: List[String], str: String): String =
      wrapWith(input.mkString(str), str)

    /**
     * The width of a Markdown column, in characters.
     */
    type Size = Int

    /**
     * The position of a Markdown column, from `0`.
     */
    type Index = Int

    private def getSizeOfIndices(
      input: List[List[String]]
    ): Map[Index, Size] = {
      def mergeMapWithMaxSize(accumulated: Map[Index, Size], current: Map[Index, Size]): Map[Index, Size] =
        current.foldLeft(Map.empty: Map[Index, Size]) { case (k, v) =>
          accumulated.get(v._1) match {
            case Some(size) => k.updated(v._1, Math.max(v._2, size))
            case None       => k.+((v._1, v._2))
          }
        }

      input.foldLeft(Map.empty: Map[Index, Size])((map, row) =>
        mergeMapWithMaxSize(
          map,
          row.zipWithIndex.map { case (string, index) => (index, string.length) }.toMap
        )
      )
    }
  }

  /**
   * The parts of a `Table` and its Markdown rendering: headings, links, rows, formats and field names.
   */
  object Table {

    /**
     * The heading of a nested table in the Markdown, named by the path of keys that leads to it.
     */
    abstract sealed case class Heading(path: List[FieldName])

    /**
     * Builds a `Heading`.
     */
    object Heading {

      /**
       * Returns the heading for `list`. An empty path gives a heading of `FieldName.Blank`, which renders as
       * `Field Descriptions`.
       */
      def mk(list: List[FieldName]): Heading =
        if (list.isEmpty) new Heading(List(FieldName.Blank)) {}
        else
          new Heading(list)                                  {}
    }

    /**
     * The Markdown text of a table cell: plain text or a link.
     */
    abstract sealed case class Link(value: String)

    /**
     * Builds a `Link`.
     */
    object Link {

      /**
       * An empty cell.
       */
      def blank: Link = new Link("") {}

      /**
       * The text `s`, unchanged.
       */
      def rawString(s: String): Link =
        new Link(s) {}

      /**
       * A Markdown link, `[name](link)`.
       */
      def githubLink(name: String, link: String): Link =
        new Link(s"[${name}](${link})") {}

      /**
       * A Confluence link, `[name|link]`.
       */
      def confluenceLink(name: String, link: String): Link =
        new Link(s"[${name}|${link}]") {}

    }

    /**
     * The link builder that `toGithubFlavouredMarkdown` passes to `toMarkdown`, and a model for writing your own. It
     * links to the GitHub anchor of the heading: the heading's key names joined together, lower-cased, with dots
     * removed and spaces turned into `-`. So the heading `x.y` gives `#xy`, and an empty path gives
     * `#field-descriptions`. A non-zero index is added as `-1`, `-2` and so on, the suffix GitHub gives a repeated
     * heading. `toMarkdown` numbers repeated headings from the last row up, so when both sides of an `orElse` use the
     * key `db`, the first row links to `#db-1`, which GitHub gives to the second `### db` table.
     */
    def githubFlavoured: (Heading, Int, Either[FieldName, Format]) => Link =
      (heading, index, fieldNameOrFormat) => {
        val headingStr =
          "#" + heading.path
            .map(_.asString(Some("Field Descriptions")))
            .mkString
            .toLowerCase
            .replace(".", "")
            .replace(" ", "-")

        val name = fieldNameOrFormat.fold(
          _.asString(Some("Field Descriptions")),
          _.asString
        )

        if (index == 0) Link.githubLink(name, headingStr)
        else Link.githubLink(name, s"${headingStr}-${index}")
      }

    // Confluence markdown
    /**
     * The link builder that `toConfluenceMarkdown` passes to `toMarkdown`. It links to the heading's key names joined
     * together, with dots and spaces removed and case kept, prefixed by `baseLink` and `-` when `baseLink` is given. It
     * ignores the heading index.
     *
     * {{{
     * import zio._
     * import zio.config._
     *
     * val link: Table.Link =
     *   Table.confluenceFlavoured(Some("https://wiki.example.com/display/OPS"))(
     *     Table.Heading.mk(List(Table.FieldName.Key("database"))),
     *     0,
     *     Right(Table.Format.AllOf)
     *   )
     * // Link([all-of|https://wiki.example.com/display/OPS-database])
     * }}}
     */
    def confluenceFlavoured(baseLink: Option[String]): (Heading, Int, Either[FieldName, Format]) => Link =
      (heading, _, fieldName) => {
        val headingStr =
          heading.path
            .map(_.asString(Some("Field Descriptions")))
            .mkString
            .replace(".", "")
            .replace(" ", "")

        val name =
          fieldName.fold(_.asString(Some("Field Descriptions")), _.asString)

        baseLink.fold(Link.confluenceLink(name, headingStr))(baseLink =>
          Link.confluenceLink(name, s"${baseLink}-${headingStr}")
        )
      }

    /**
     * Returns a table with the single row `tableRow`.
     */
    def singletonTable(tableRow: TableRow): Table =
      Table(List(tableRow))

    /**
     * One row of a `Table`, shown as one line of a Markdown table.
     *
     * @param paths
     *   the keys that lead to this row, such as `aws`, `ec2`, `type` for the key `aws.ec2.type`. In a nested table they
     *   can start below the root, and they are empty for a row that groups keys without a key of its own.
     * @param format
     *   the shape of the value: a primitive, a list, a map, nested keys, or a group of keys (`AllOf`, `AnyOneOf`)
     * @param description
     *   the descriptions of the key: the built-in one, such as `an integer property`, and those added with `??`
     * @param nested
     *   the table of the keys under this row, when it has any
     * @param sources
     *   the names shown in the `Sources` column; `toTable` leaves it empty
     */
    case class TableRow(
      paths: List[FieldName],
      format: Option[Format],
      description: List[ConfigDocs.Description],
      nested: Option[Table],
      sources: Set[String]
    ) {
      // A single row can be turned to a table
      /**
       * Returns a table with this row only.
       */
      def asTable: Table =
        singletonTable(this)
    }

    /**
     * The shape of the value at a row's key, shown in the `Format` column. `Primitive` is a single value; `List` and
     * `Map` are collections; `Nested` is a key that holds other keys. `AllOf` means the keys under it must all be
     * given, as built by `zip`, and `AnyOneOf` means one of the alternatives under it must be given, as built by
     * `orElse`. `NotApplicable` renders as an empty cell. `Recursion` and `RecursionList` name recursive configs, but
     * `toTable` doesn't produce them.
     */
    sealed trait Format { self =>

      /**
       * Returns the text of the `Format` column: `list`, `map`, `primitive`, `nested`, `any-one-of`, `all-of`,
       * `recursion`, `list of recursion`, or an empty string for `NotApplicable`.
       */
      def asString: String =
        self match {
          case Format.List          => "list"
          case Format.Map           => "map"
          case Format.Primitive     => "primitive"
          case Format.Nested        => "nested"
          case Format.AnyOneOf      => "any-one-of"
          case Format.AllOf         => "all-of"
          case Format.Recursion     => "recursion"
          case Format.RecursionList => "list of recursion"
          case Format.NotApplicable => ""
        }
    }

    /**
     * The cases of `Format`.
     */
    object Format {

      /**
       * A list of values.
       */
      case object List extends Format

      /**
       * A map from keys found in the source to values. A config built with `switch` also gets this format.
       */
      case object Map extends Format

      /**
       * A single value, such as a string or a number.
       */
      case object Primitive extends Format

      /**
       * A key that holds other keys.
       */
      case object Nested extends Format

      /**
       * Alternatives, one of which must be given.
       */
      case object AnyOneOf extends Format

      /**
       * Keys that must all be given.
       */
      case object AllOf extends Format

      /**
       * No format; renders as an empty cell.
       */
      case object NotApplicable extends Format

      /**
       * A recursive config. `toTable` doesn't produce it.
       */
      case object Recursion extends Format

      /**
       * A list of a recursive config. `toTable` produces it only for a list whose element has the format `Recursion`,
       * which `toTable` doesn't produce.
       */
      case object RecursionList extends Format
    }

    /**
     * The text of a row's `FieldName` column: a key, or blank.
     */
    sealed trait FieldName {

      /**
       * Returns the key, or for `Blank` the text in `forBlank` (an empty string when it is `None`).
       */
      def asString(forBlank: Option[String]): String =
        this match {
          case FieldName.Key(k) => k
          case FieldName.Blank  => forBlank.getOrElse("")
        }
    }

    /**
     * The cases of `FieldName`.
     */
    object FieldName {

      /**
       * A key name.
       */
      case class Key(k: String) extends FieldName

      /**
       * No key, for a row that groups keys without a key of its own.
       */
      case object Blank extends FieldName
    }
  }

  /**
   * Builds the documentation of `config`: its keys, the built-in description of each value (such as
   * `an integer property`), the descriptions added with `??`, and how the keys combine. Convert the result with
   * `toTable` and render that as Markdown. The table shows the key that a `switch` reads, with the format `map`, and
   * leaves out its branches.
   *
   * `generateDocs` does not support recursive configs built with `Config.defer`.
   *
   * {{{
   * import zio._
   * import zio.config._
   *
   * final case class Database(host: String, port: Int)
   *
   * val database: Config[Database] =
   *   (Config.string("host") ?? "Database host").zip(Config.int("port") ?? "Database port").to[Database]
   *
   * val markdown: String = generateDocs(database.nested("database")).toTable.toGithubFlavouredMarkdown
   * }}}
   *
   * @see
   *   [[https://zio.dev/zio-config/dive-into-zio-config/ Dive Into ZIO Config]]
   */
  final def generateDocs[A](config: zio.Config[A]): ConfigDocs = {
    def loop[B](
      descriptions: List[ConfigDocs.Description],
      config: Config[B],
      latestPath: Option[String],
      alreadySeen: Set[Config[_]]
    ): ConfigDocs =
      config match {
        case Config.Lazy(thunk) =>
          loop(descriptions, thunk(), latestPath, alreadySeen + thunk())

        case cp: Config.Primitive[_] =>
          ConfigDocs.Leaf(ConfigDocs.Description(latestPath, cp.description) :: descriptions, None)

        case cd: Config.Table[_] =>
          ConfigDocs.DynamicMap(
            loop(
              descriptions,
              cd.valueConfig,
              None,
              alreadySeen + config
            )
          )

        case Config.Optional(c) =>
          loop(descriptions, c, None, alreadySeen)

        case Config.Sequence(c) =>
          ConfigDocs.Sequence(
            loop(
              descriptions,
              c,
              None,
              alreadySeen
            )
          )

        case Config.Switch(c, map) =>
          ConfigDocs.DynamicMap(
            loop(descriptions, c, latestPath, alreadySeen),
            map.map { case (k, v) =>
              k.toString -> loop(descriptions, v, latestPath, alreadySeen)
            }
          )

        case Config.Described(c, desc) =>
          val descri: ConfigDocs.Description =
            ConfigDocs.Description(latestPath, desc)

          loop(
            descri :: descriptions,
            c,
            latestPath,
            alreadySeen
          )

        case Config.Nested(path, c) =>
          ConfigDocs.Nested(
            path,
            loop(
              List.empty,
              c,
              Some(path),
              alreadySeen
            ),
            descriptions
          )

        case Config.MapOrFail(c, _) =>
          loop(descriptions, c, None, alreadySeen)

        case Config.Zipped(left, right, _) =>
          ConfigDocs.Zip(
            loop(descriptions, left, None, alreadySeen),
            loop(descriptions, right, None, alreadySeen)
          )

        case a: Config.Fallback[_] =>
          ConfigDocs.OrElse(
            loop(descriptions, a.first, None, alreadySeen),
            loop(descriptions, a.second, None, alreadySeen)
          )

      }

    loop(Nil, config, None, Set.empty)
  }

}
