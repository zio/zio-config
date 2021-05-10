package zio.config

trait ConfigDocsModule extends WriteModule {
  import ConfigDescriptorAdt._
  import Table._

  /**
   * `ConfigDocs` holds the descriptions and details of a `ConfigDescriptor`
   * which can be used to produce documentation.
   */
  sealed trait ConfigDocs { self =>
    private def is[A](f: PartialFunction[ConfigDocs, A])(orElse: A): A =
      f.applyOrElse(self, (_: ConfigDocs) => orElse)

    /**
     * Convert a {{{ ConfigDocs }}} to a {{{ Table }}}.
     *
     * A Table is a recursive structure that is more easier to be interpreted as Json or Markdown than trying
     * to convert `ConfigDocs` to a readable format.
     */
    def toTable: Table = {
      def filterDescriptions(
        descriptionsUsedAlready: Option[Table.FieldName],
        descriptions: List[ConfigDocs.Description]
      ) = {
        val desc =
          descriptionsUsedAlready match {
            case Some(value) =>
              descriptions.filter({
                case ConfigDocs.Description(path, _) if path.map(FieldName.Key.apply) == Some(value) =>
                  false
                case ConfigDocs.Description(_, _)                                                    => true
              })
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
          case ConfigDocs.Leaf(sources, descriptions, _) =>
            val desc = filterDescriptions(descriptionsUsedAlready, descriptions)

            TableRow(
              previousPaths,
              Some(Table.Format.Primitive),
              desc,
              None,
              sources.map(_.name)
            ).asTable
          case ConfigDocs.Recursion(sources)             =>
            TableRow(
              previousPaths,
              Some(Table.Format.Recursion),
              List.empty,
              None,
              sources.map(_.name)
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
    sealed case class Description(path: Option[K], description: String)

    def findByPath(description: List[Description], path: FieldName): List[Description] =
      description
        .flatMap(desc =>
          desc match {
            case a @ ConfigDocs.Description(p, _) if (p.map(FieldName.Key.apply)) == Some(path) =>
              List(a)
            case ConfigDocs.Description(_, _)                                                   => Nil
          }
        )

    sealed case class Leaf(sources: Set[ConfigSourceName], descriptions: List[Description], value: Option[V] = None)
        extends ConfigDocs
    sealed case class Recursion(sources: Set[ConfigSourceName])                                  extends ConfigDocs
    sealed case class Nested(path: K, docs: ConfigDocs, descriptions: List[Description])         extends ConfigDocs
    sealed case class Zip(left: ConfigDocs, right: ConfigDocs)                                   extends ConfigDocs
    sealed case class OrElse(leftDocs: ConfigDocs, rightDocs: ConfigDocs)                        extends ConfigDocs
    sealed case class Sequence(schemaDocs: ConfigDocs, valueDocs: List[ConfigDocs] = List.empty) extends ConfigDocs
    sealed case class DynamicMap(schemaDocs: ConfigDocs, valueDocs: Map[K, ConfigDocs] = Map.empty[K, ConfigDocs])
        extends ConfigDocs

  }

  /**
   * A Table is a recursive structure that is more easier to be interpreted as Json or Markdown than trying
   * to convert `ConfigDocs` to a readable format.
   *
   * @param rows: A table consist of multiple `TableRow`s where each `TableRow` holds the information about
   *            the config path.
   */
  sealed case class Table(rows: List[TableRow]) { self =>
    def ++(that: Table): Table =
      Table(rows ++ that.rows)

    /**
     * Create a Confluence flavored markdown string from Table.
     * This can be used if you are planning to render this markdown in Atlassian's Confluence pages.
     *
     * @param baseUrl: Every heading in a markdown rendered through Atlassian's Confluence page needs to have a baseUrl.
     *               This can be the baseUrl of the confluence page in which markdown is rendered.
     *               The heading in markdown will be the keys of your application config.
     */
    def toConfluenceMarkdown(
      baseUrl: Option[String]
    )(implicit S: K <:< String): String =
      toMarkdown(Table.confluenceFlavoured(baseUrl))

    /**
     * Create a Github flavored markdown string from Table.
     * This can be used to render markdowns in Github, Gitlab etc
     */
    def toGithubFlavouredMarkdown(implicit S: K <:< String): String =
      toMarkdown(Table.githubFlavoured)

    def toMarkdown(
      getLink: (Heading, Int, Either[FieldName, Format]) => Link
    )(implicit S: K <:< String): String = {
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

                case None    =>
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

    def mapFormat(f: Option[Format] => Format): Table =
      Table(rows.map(row => row.copy(format = Some(f(row.format)))))

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

    type Size  = Int
    type Index = Int

    private def getSizeOfIndices(
      input: List[List[String]]
    ): Map[Index, Size] = {
      def mergeMapWithMaxSize(accumulated: Map[Index, Size], current: Map[Index, Size]): Map[Index, Size] =
        current.foldLeft(Map.empty: Map[Index, Size])({ case (k, v) =>
          accumulated.get(v._1) match {
            case Some(size) => k.updated(v._1, Math.max(v._2, size))
            case None       => k.+((v._1, v._2))
          }
        })

      input.foldLeft(Map.empty: Map[Index, Size])((map, row) =>
        mergeMapWithMaxSize(
          map,
          row.zipWithIndex
            .map({ case (string, index) => (index, string.length) })
            .toMap
        )
      )
    }
  }

  object Table {

    abstract sealed case class Heading(path: List[FieldName])

    object Heading {
      def mk(list: List[FieldName]): Heading =
        if (list.isEmpty) new Heading(List(FieldName.Blank)) {}
        else
          new Heading(list)                                  {}
    }

    abstract sealed case class Link(value: String)

    object Link {

      def blank: Link = new Link("") {}

      def rawString(s: String): Link                       =
        new Link(s) {}

      def githubLink(name: String, link: String): Link     =
        new Link(s"[${name}](${link})") {}

      def confluenceLink(name: String, link: String): Link =
        new Link(s"[${name}|${link}]") {}

    }

    /**
     * Internal function that represents the creation of a github flavoured markdown. The implementation can be used as reference for users
     * who would like to produce a different style markdown rendering by specifying
     * how to create `Link` given a `Heading`, `Int` representing the index of the key (or paths) and `Either[FieldName, Format]`.
     *
     * The index exists because it represents the index of a heading (which is the individual key of paths) in markdown. This is usually zero for all headings unless
     * there are duplicate headings in the markdown. There is a possibility of duplicate headings in the markdown, if for instance,
     * given a path `x.y` and `k.y`, the heading `y` can appear twice in the markdown file with indices as 0 and 1. Depending on the flavour of markdown (Example: Github, Confluence)
     * we have different ways to produce links towards those headings. In this case, we employ the strategy used by Github.
     */
    def githubFlavoured(implicit
      S: K <:< String
    ): (Heading, Int, Either[FieldName, Format]) => Link =
      (heading, index, fieldNameOrFormat) => {
        val headingStr =
          heading.path
            .map(_.asString(Some("Field Descriptions")))
            .mkString
            .toLowerCase
            .replace(".", "")
            .replace(" ", "")

        val name = fieldNameOrFormat.fold(
          _.asString(Some("Field Descriptions")),
          _.asString
        )

        if (index == 0) Link.githubLink(name, headingStr)
        else Link.githubLink(name, s"${headingStr}-${index}")
      }

    // Confluence markdown
    def confluenceFlavoured(baseLink: Option[String])(implicit
      S: K <:< String
    ): (Heading, Int, Either[FieldName, Format]) => Link =
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

    def singletonTable(tableRow: TableRow): Table =
      Table(List(tableRow))

    /**
     * A `TableRow` represents each row in a `Table` which is an intermediate light-weight structure produced from `ConfigDocs`.
     * `Table` is more easier to be converted to formats such as Json, markdown or any custom format fo your choice.
     *
     * @param paths : Each config key is basically a list of paths representing its hierarchy. Example: "aws.ec2.instance.type" where list of paths is
     *              List("aws", "ec2", "instance", "type")
     * @param format : The format of value of key (paths). Example: it can be a Primitive type (String, Int etc), or it can be complex structures as such as List or Map.
     * @param description : Description (zio-config in-built or user-provided) of the key (paths).
     * @param nested : A `TableRow` can be pointed to a nested table that has the details of all the child paths that are under `paths`. Hence `TableRow` is a recursive structure.
     * @param sources: All the sources from which `paths` can be retrieved.
     */
    case class TableRow(
      paths: List[FieldName],
      format: Option[Format],
      description: List[ConfigDocs.Description],
      nested: Option[Table],
      sources: Set[String]
    ) {
      // A single row can be turned to a table
      def asTable: Table =
        singletonTable(this)
    }

    /**
     * Format is further used in `Table` which is used for config documentation. Format helps the readers of the documentation understand the details of the format
     * of each paths that forms their application config.
     *
     *  Example: A format can be `List`, `Map`, `Primitive`, or it can even be even more complex such as `AllOf` or `AnyOneOf`.
     *  If `Format` of paths `K` is `AllOf`, it implies that there are more distinct paths under the paths `K`, and user need to satisfy (i.e, provide them in the source)
     *  all of the paths under `K`.
     *
     *  If `Format` of paths `K` is `AnyOneOf`, it implies there are more distinct paths under the paths `K`, then user need to satisfy (i.e, provide them in the source)
     *  any one of the paths under `K`.
     *
     * If `Format` of oaths `K` is `Recursion` then that means there is a repetition of same path structure under the paths `K`
     */
    sealed trait Format { self =>
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

    object Format {
      case object List          extends Format
      case object Map           extends Format
      case object Primitive     extends Format
      case object Nested        extends Format
      case object AnyOneOf      extends Format
      case object AllOf         extends Format
      case object NotApplicable extends Format
      case object Recursion     extends Format
      case object RecursionList extends Format
    }

    sealed trait FieldName {
      def asString(forBlank: Option[String])(implicit S: K <:< String): String =
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

  import ConfigDocs.{DynamicMap => DocsMap, Leaf => DocsLeaf}

  /**
   * Generate documentation based on the `ConfigDescriptor`, where a
   * `ConfigDescriptor` is a structure representing the logic to fetch the application config
   * from various sources.
   *
   * Once we generate the docs, this can be converted to a light weight `Table` structure which is much more easier to be converted
   * to markdown or json formats.
   *
   * Example :
   * {{{
   *   val configDescriptor: ConfigDescriptor[MyAppConfig] = ???
   *
   *   generatedDocs(configDescriptor).toTable.toGithubFlavouredMarkdown
   * }}}
   */
  final def generateDocs[A](config: ConfigDescriptor[A]): ConfigDocs = {
    def loopTo[B](
      sources: Set[ConfigSourceName],
      descriptions: List[ConfigDocs.Description],
      config: ConfigDescriptor[B],
      latestPath: Option[K],
      alreadySeen: Set[ConfigDescriptor[_]]
    ): ConfigDocs =
      if (alreadySeen.contains(config)) {
        ConfigDocs.Recursion(sources)
      } else {
        loop(sources, descriptions, config, latestPath, alreadySeen + config)
      }

    def loop[B](
      sources: Set[ConfigSourceName],
      descriptions: List[ConfigDocs.Description],
      config: ConfigDescriptor[B],
      latestPath: Option[K],
      alreadySeen: Set[ConfigDescriptor[_]]
    ): ConfigDocs =
      config match {
        case Lazy(thunk) =>
          loopTo(sources, descriptions, thunk(), latestPath, alreadySeen)

        case Source(source, _) =>
          DocsLeaf((source.names ++ sources), descriptions, None)

        case Default(c, _) =>
          loopTo(sources, descriptions, c, None, alreadySeen)

        case cd: DynamicMap[_] =>
          ConfigDocs.DynamicMap(
            loopTo(
              (cd.source.names ++ sources),
              descriptions,
              cd.config,
              None,
              alreadySeen
            )
          )

        case Optional(c) =>
          loopTo(sources, descriptions, c, None, alreadySeen)

        case Sequence(source, c) =>
          ConfigDocs.Sequence(
            loopTo(
              (source.names ++ sources),
              descriptions,
              c,
              None,
              alreadySeen
            )
          )

        case Describe(c, desc) =>
          val descri: ConfigDocs.Description =
            ConfigDocs.Description(latestPath, desc)

          loopTo(
            sources,
            descri :: descriptions,
            c,
            latestPath,
            alreadySeen
          )

        case Nested(source, path, c) =>
          ConfigDocs.Nested(
            path,
            loopTo(
              source.names ++ sources,
              List.empty,
              c,
              Some(path),
              alreadySeen
            ),
            descriptions
          )

        case TransformOrFail(c, _, _) =>
          loopTo(sources, descriptions, c, None, alreadySeen)

        case Zip(left, right) =>
          ConfigDocs.Zip(
            loopTo(sources, descriptions, left, None, alreadySeen),
            loopTo(sources, descriptions, right, None, alreadySeen)
          )

        case OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loopTo(sources, descriptions, left, None, alreadySeen),
            loopTo(sources, descriptions, right, None, alreadySeen)
          )

        case OrElse(left, right) =>
          ConfigDocs.OrElse(
            loopTo(sources, descriptions, left, None, alreadySeen),
            loopTo(sources, descriptions, right, None, alreadySeen)
          )
      }

    loopTo(Set.empty, Nil, config, None, Set.empty)
  }

  /**
   * Generate a report based on the `ConfigDescriptor` and an `A`, where a
   * `ConfigDescriptor` represents the logic to fetch the application config
   * from various sources, and `A` represents the actual config value that was retrieved.
   */
  def generateReport[A](config: ConfigDescriptor[A], value: A): Either[String, ConfigDocs] =
    write[A](config, value).map { tree =>
      def loop(tree: PropertyTree[K, V], schemaDocs: ConfigDocs, keys: List[K]): ConfigDocs =
        schemaDocs match {
          case DocsLeaf(sources, descriptions, None) =>
            // Feed value when it hits leaf
            tree.getPath(keys) match {
              case PropertyTree.Leaf(value) =>
                DocsLeaf(sources, descriptions, Some(value))
              case _                        => DocsLeaf(sources, descriptions, None)
            }

          case a: DocsLeaf => a

          case a: ConfigDocs.Recursion => a

          case ConfigDocs.Nested(path, docs, descriptions) =>
            ConfigDocs
              .Nested(path, loop(tree, docs, keys :+ path), descriptions)

          case ConfigDocs.Zip(left, right) =>
            ConfigDocs.Zip(loop(tree, left, keys), loop(tree, right, keys))

          case ConfigDocs.OrElse(left, right) =>
            ConfigDocs.OrElse(loop(tree, left, keys), loop(tree, right, keys))

          case cd: DocsMap =>
            tree.getPath(keys) match {
              case rec: PropertyTree.Record[K, V] =>
                DocsMap(
                  cd.schemaDocs,
                  rec.value.toList.map { keyTree =>
                    keyTree._1 -> loop(keyTree._2, cd.schemaDocs, List.empty)
                  }.toMap
                )
              case v                              =>
                DocsMap(
                  loop(v, cd.schemaDocs, keys),
                  Map.empty[K, ConfigDocs]
                )
            }

          case ConfigDocs.Sequence(schema, values) =>
            tree.getPath(keys) match {
              case PropertyTree.Sequence(value) if value.nonEmpty =>
                ConfigDocs.Sequence(
                  schema,
                  value.map(t => loop(t, schema, List.empty))
                )
              case _                                              =>
                ConfigDocs
                  .Sequence(schema, loop(tree, schema, keys) :: values)
            }
        }

      loop(tree, generateDocs(config), List.empty)
    }
}
