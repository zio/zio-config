package zio.config

trait ConfigDocsModule extends WriteModule {
  import ConfigDescriptorAdt._
  import Table._

  sealed trait ConfigDocs { self =>
    private def is[A](f: PartialFunction[ConfigDocs, A])(orElse: A): A =
      f.applyOrElse(self, (_: ConfigDocs) => orElse)

    /**
     * Convert ConfigDocs to `Table`, which is a light-weight and flattened, yet
     * recursive structure, that can then be easily turned into human readable formats such as markdown, table, html etc
     */
    def toTable: Table = {
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
              go(right, previousPaths, Some(currentNode), descriptionsUsedAlready)
          } else {
            val tableWithAllDescriptions =
              (go(left, previousPaths, Some(currentNode), descriptionsUsedAlready) ++
                go(right, previousPaths, Some(currentNode), descriptionsUsedAlready))

            // Look backwards and see if previous node is nested, if so remove the already used descriptions
            val parentDoc =
              if (previousNode.exists(_.is { case ConfigDocs.Nested(_, _) => true }(false))) {
                previousPaths.lastOption match {
                  case Some(value) =>
                    ConfigDocs
                      .findByPath(
                        tableWithAllDescriptions.rows.flatMap(_.description),
                        value
                      )
                      .distinct
                  case None => Nil
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
          case ConfigDocs.Leaf(sources, descriptions, _) =>
            val desc =
              descriptionsUsedAlready match {
                case Some(value) =>
                  descriptions.filter({
                    case ConfigDocs.Description(path, _) if path.map(FieldName.Key) == Some(value) =>
                      false
                    case ConfigDocs.Description(_, _) => true
                  })
                case None => descriptions
              }

            TableRow(previousPaths, Some(Table.Format.Primitive), desc, None, sources.map(_.name)).asTable

          case c @ ConfigDocs.Nested(path, docs) =>
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
            if (docs.is {
                  case ConfigDocs.Nested(_, _) => true
                }(false)) {
              TableRow(
                previousPaths :+ Table.FieldName.Key(path),
                Some(Table.Format.Nested),
                Nil,
                Some(result),
                Set.empty
              ).asTable
            } else
              result

          case c @ ConfigDocs.Zip(left, right) =>
            handleNested(left, right, c, _.is { case ConfigDocs.Zip(_, _) => true }(false), Format.AllOf)

          case c @ ConfigDocs.OrElse(left, right) =>
            handleNested(left, right, c, _.is { case ConfigDocs.OrElse(_, _) => true }(false), Format.AnyOneOf)

          case c @ ConfigDocs.Sequence(schemaDocs, _) =>
            go(schemaDocs, previousPaths, Some(c), descriptionsUsedAlready)
              .setFormatGlobally(Format.List)

          case c @ ConfigDocs.DynamicMap(schemaDocs, _) =>
            go(schemaDocs, previousPaths, Some(c), descriptionsUsedAlready)
              .setFormatGlobally(Format.Map)
        }
      }

      go(self, Nil, None, None)
    }
  }

  object ConfigDocs {
    case class Description(path: Option[K], description: String)

    def findByPath(description: List[Description], path: FieldName): List[Description] =
      description
        .flatMap(
          desc =>
            desc match {
              case a @ ConfigDocs.Description(p, _) if (p.map(FieldName.Key)) == Some(path) => List(a)
              case ConfigDocs.Description(_, _)                                             => Nil
            }
        )

    case class Leaf(sources: Set[ConfigSourceName], descriptions: List[Description], value: Option[V] = None)
        extends ConfigDocs

    case class Nested(path: K, docs: ConfigDocs)                                          extends ConfigDocs
    case class Zip(left: ConfigDocs, right: ConfigDocs)                                   extends ConfigDocs
    case class OrElse(leftDocs: ConfigDocs, rightDocs: ConfigDocs)                        extends ConfigDocs
    case class Sequence(schemaDocs: ConfigDocs, valueDocs: List[ConfigDocs] = List.empty) extends ConfigDocs
    case class DynamicMap(schemaDocs: ConfigDocs, valueDocs: Map[K, ConfigDocs] = Map.empty[K, ConfigDocs])
        extends ConfigDocs

  }

  /**
   * @param rows: A table consist of multiple rows, where each row is a field and it's details.
   */
  case class Table(rows: List[TableRow]) { self =>
    def setFormatGlobally(format: Format): Table =
      Table(rows.map(_.copy(format = Some(format))))

    def ++(that: Table): Table =
      Table(rows ++ that.rows)

    def asGithubFlavouredMarkdown(implicit S: K =:= String): String =
      asMarkdown(Table.githubFlavoured)

    def asConfluenceMarkdown(baseUrl: Option[String])(implicit S: K =:= String): String =
      asMarkdown(Table.confluenceFlavoured(baseUrl))

    def asMarkdown(
      getLink: (Heading, Int, Either[FieldName, Format]) => Link
    )(implicit S: K =:= String): String = {
      val headingColumns =
        List(
          "FieldName",
          "Format",
          "Description",
          "Sources"
        )

      def updateHeadingAndIndex(heading: Heading, map: Map[Heading, Int]): Map[Heading, Int] = {
        val index = map.get(heading).map(index => index + 1).getOrElse(0)
        map.updated(heading, index)
      }

      def convertHeadingToString(paths: List[FieldName]): String =
        paths.map(_.asString(Some("Field Descriptions"))).mkString(".")

      def go(table: Table, usedHeadings: Map[Heading, Int]): List[String] = {
        val (contents, nestedTables, updatedUsedHeadings): (
          List[List[String]],
          List[(Table, Heading)],
          Map[Heading, Int]
        ) =
          table.rows
            .foldRight((List.empty[List[String]], List.empty[(Table, Heading)], usedHeadings)) {
              case (row, (contentList, nestedTableList, usedHeadings)) =>
                val lastFieldName = row.previousPaths.lastOption.getOrElse(FieldName.Blank)

                val formatOrNotApplicable = row.format.getOrElse(Format.NotApplicable)
                val heading               = Heading.mk(row.previousPaths)
                val updatedHeading        = updateHeadingAndIndex(heading, usedHeadings)

                val (name, format) = row.nested match {
                  case Some(_) =>
                    val getLinkFn =
                      (s: Either[FieldName, Format]) => getLink(heading, updatedHeading.getOrElse(heading, 0), s)

                    val nameWithLink =
                      lastFieldName match {
                        case FieldName.Key(_) => getLinkFn(Left(lastFieldName))
                        case FieldName.Blank  => Link.rawString(lastFieldName.asString(None))
                      }

                    nameWithLink -> getLinkFn(Right(formatOrNotApplicable))

                  case None =>
                    Link.rawString(lastFieldName.asString(None)) -> Link.rawString(formatOrNotApplicable.asString)
                }

                (
                  List(
                    name.value,
                    format.value,
                    row.description.map(_.description).mkString(", "),
                    row.sources.mkString(", ")
                  ) :: contentList,
                  row.nested.map(table => (table, heading)).toList ++ nestedTableList,
                  updatedHeading
                )
            }

        val contentList: List[String] = {
          val indexAndSize = getSizeOfIndices(headingColumns :: contents)

          (headingColumns :: List.fill(indexAndSize.size)("---") :: contents).map(
            fieldValues =>
              mkStringAndWrapWith(fieldValues.zipWithIndex.map {
                case (string, index) =>
                  padToEmpty(string, indexAndSize.getOrElse(index, 0))
              }, "|")
          )
        }

        contentList.mkString(System.lineSeparator()) ::
          nestedTables.flatMap(
            table =>
              mkStringAndWrapWith(
                List(s"### ${convertHeadingToString(table._2.path)}"),
                System.lineSeparator()
              ) :: go(table._1, updatedUsedHeadings)
          )
      }

      mkStringAndWrapWith(
        s"## Configuration Details" :: (System.lineSeparator() :: go(self, Map.empty)),
        System.lineSeparator()
      )
    }

    private def padToEmpty(string: String, size: Int): String = {
      val maxSize = Math.max(string.length, size)
      string.padTo(maxSize, ' ')
    }

    private def wrapWith(input: String, str: String): String =
      str ++ input ++ str

    private def mkStringAndWrapWith(input: List[String], str: String): String =
      wrapWith(input.mkString(str), str)

    type Size  = Int
    type Index = Int

    private def getSizeOfIndices(input: List[List[String]]): Map[Index, Size] = {
      def mergeMapWithMaxSize(accumulated: Map[Index, Size], current: Map[Index, Size]): Map[Index, Size] =
        current.foldLeft(Map.empty: Map[Index, Size])({
          case (k, v) =>
            accumulated.get(v._1) match {
              case Some(size) => k.updated(v._1, Math.max(v._2, size))
              case None       => k.+((v._1, v._2))
            }
        })

      input.foldLeft(Map.empty: Map[Index, Size])(
        (map, row) =>
          mergeMapWithMaxSize(map, row.zipWithIndex.map({ case (string, index) => (index, string.length) }).toMap)
      )
    }
  }

  object Table {

    abstract sealed case class Heading(path: List[FieldName])

    object Heading {
      def mk(list: List[FieldName]): Heading =
        if (list.isEmpty) new Heading(List(FieldName.Blank)) {}
        else new Heading(list)                               {}
    }

    abstract sealed case class Link(value: String)

    object Link {

      def blank: Link = new Link("") {}

      def rawString(s: String): Link =
        new Link(s) {}

      def githubLink(name: String, link: String): Link =
        new Link(s"[${name}](${link})") {}

      def confluenceLink(name: String, link: String): Link =
        new Link(s"[${name}|${link}]") {}

    }

    // GFM
    def githubFlavoured(implicit S: K =:= String): (Heading, Int, Either[FieldName, Format]) => Link =
      (heading, index, fieldNameOrFormat) => {
        val headingStr =
          heading.path
            .map(_.asString(Some("Field Descriptions")))
            .mkString
            .toLowerCase
            .replace(".", "")
            .replace(" ", "")

        val name = fieldNameOrFormat.fold(_.asString(Some("Field Descriptions")), _.asString)

        if (index == 0) Link.githubLink(name, headingStr) else Link.githubLink(name, s"${headingStr}-${index}")
      }

    // Confluence markdown
    def confluenceFlavoured(
      baseLink: Option[String]
    )(implicit S: K =:= String): (Heading, Int, Either[FieldName, Format]) => Link =
      (heading, _, fieldName) => {
        val headingStr =
          heading.path.map(_.asString(Some("Field Descriptions"))).mkString.replace(".", "").replace(" ", "")

        val name = fieldName.fold(_.asString(Some("Field Descriptions")), _.asString)

        baseLink.fold(Link.confluenceLink(name, headingStr))(
          baseLink => Link.confluenceLink(name, s"${baseLink}-${headingStr}")
        )
      }

    def singletonTable(tableRow: TableRow) =
      Table(List(tableRow))

    case class TableRow(
      previousPaths: List[FieldName],
      format: Option[Format],
      description: List[ConfigDocs.Description],
      nested: Option[Table],
      sources: Set[String]
    ) {
      // A single row can be turned to a table
      def asTable: Table =
        singletonTable(this)
    }

    sealed trait Format { self =>
      def asString: String =
        self match {
          case Format.List          => "list"
          case Format.Map           => "map"
          case Format.Primitive     => "primitive"
          case Format.Nested        => "nested"
          case Format.AnyOneOf      => "any-one-of"
          case Format.AllOf         => "all-of"
          case Format.NotApplicable => ""
        }
    }

    object Format {
      case object List          extends Format
      case object Map           extends Format
      case object Primitive     extends Format
      case object Nested        extends Format
      case object AnyOneOf      extends Format
      case object AllOf         extends Format
      case object NotApplicable extends Format
    }

    sealed trait FieldName {
      def asString(forBlank: Option[String])(implicit S: K =:= String): String =
        this match {
          case FieldName.Key(k) => S.apply(k)
          case FieldName.Blank  => forBlank.getOrElse("")
        }
    }

    object FieldName {
      case class Key(k: K) extends FieldName
      case object Blank    extends FieldName
    }
  }

  import ConfigDocs.{ DynamicMap => DocsMap, Leaf => DocsLeaf }

  /**
   * Generate documentation based on the `ConfigDescriptor`, where a
   * `ConfigDescriptor` is a structure representing the logic to fetch the application config
   * from various sources.
   */
  final def generateDocs[A](config: ConfigDescriptor[A]): ConfigDocs = {
    def loop[B](
      sources: Set[ConfigSourceName],
      descriptions: List[ConfigDocs.Description],
      config: ConfigDescriptor[B],
      latestPath: Option[K]
    ): ConfigDocs =
      config match {
        case Source(source, _) =>
          DocsLeaf((source.names ++ sources), descriptions, None)

        case Default(c, _) =>
          loop(sources, descriptions, c, None)

        case cd: DynamicMap[_] =>
          ConfigDocs.DynamicMap(
            loop((cd.source.names ++ sources), descriptions, cd.config, None)
          )

        case Optional(c) =>
          loop(sources, descriptions, c, None)

        case Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop((source.names ++ sources), descriptions, c, None)
          )

        case Describe(c, desc) =>
          val descri: ConfigDocs.Description =
            ConfigDocs.Description(latestPath, desc)

          loop(sources, descri :: descriptions, c, latestPath)

        case Nested(path, c) =>
          ConfigDocs.Nested(path, loop(sources, descriptions, c, Some(path)))

        case XmapEither(c, _, _) =>
          loop(sources, descriptions, c, None)

        case Zip(left, right) =>
          ConfigDocs.Zip(
            loop(sources, descriptions, left, None),
            loop(sources, descriptions, right, None)
          )

        case OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, None),
            loop(sources, descriptions, right, None)
          )

        case OrElse(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, None),
            loop(sources, descriptions, right, None)
          )
      }

    loop(Set.empty, Nil, config, None)
  }

  /**
   * Generate a report based on the `ConfigDescriptor` and an `A`, where a
   * `ConfigDescriptor` represents the logic to fetch the application config
   * from various sources, and `A` represents the actual config value that was retrieved.
   */
  def generateReport[A](
    config: ConfigDescriptor[A],
    value: A
  ): Either[String, ConfigDocs] =
    write[A](config, value)
      .map(tree => {
        def loop(
          tree: PropertyTree[K, V],
          schemaDocs: ConfigDocs,
          keys: List[K]
        ): ConfigDocs =
          schemaDocs match {
            case DocsLeaf(sources, descriptions, None) =>
              // Feed value when it hits leaf
              tree.getPath(keys) match {
                case PropertyTree.Leaf(value) => DocsLeaf(sources, descriptions, Some(value))
                case _                        => DocsLeaf(sources, descriptions, None)
              }

            case a: DocsLeaf => a

            case ConfigDocs.Nested(path, docs) =>
              ConfigDocs.Nested(path, loop(tree, docs, keys :+ path))

            case ConfigDocs.Zip(left, right) =>
              ConfigDocs.Zip(loop(tree, left, keys), loop(tree, right, keys))

            case ConfigDocs.OrElse(left, right) =>
              ConfigDocs.OrElse(loop(tree, left, keys), loop(tree, right, keys))

            case cd: DocsMap =>
              tree.getPath(keys) match {
                case rec: PropertyTree.Record[K, V] =>
                  DocsMap(cd.schemaDocs, rec.value.toList.map { keyTree =>
                    keyTree._1 -> loop(keyTree._2, cd.schemaDocs, List.empty)
                  }.toMap)
                case v => DocsMap(loop(v, cd.schemaDocs, keys), Map.empty[K, ConfigDocs])
              }

            case ConfigDocs.Sequence(schema, values) =>
              tree.getPath(keys) match {
                case PropertyTree.Sequence(value) if value.nonEmpty =>
                  ConfigDocs.Sequence(schema, value.map(t => loop(t, schema, List.empty)))
                case _ =>
                  ConfigDocs.Sequence(schema, loop(tree, schema, keys) :: values)
              }
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
