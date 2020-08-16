package zio.config

trait ConfigDocsModule extends WriteModule {
  import ConfigDescriptorAdt._
  import Table._

  sealed trait ConfigDocs { self =>
    private def is[A](f: PartialFunction[ConfigDocs, A])(orElse: A): A =
      f.applyOrElse(self, (_: ConfigDocs) => orElse)

    private def hasNested: Boolean = self match {
      case ConfigDocs.Leaf(sources, descriptions, value) => false
      case ConfigDocs.Nested(_, _)                       => true
      case ConfigDocs.Zip(left, right)                   => left.hasNested || right.hasNested
      case ConfigDocs.OrElse(leftDocs, rightDocs)        => leftDocs.hasNested || rightDocs.hasNested
      case ConfigDocs.Sequence(schemaDocs, valueDocs)    => false
      case ConfigDocs.DynamicMap(schemaDocs, valueDocs)  => false
    }

    /**
     * Convert ConfigDocs to `Table`, which is a light-weight and flattened, yet
     * recursive structure, that can then be easily turned into human readable formats such as markdown, table, html etc
     */
    def toTable(implicit M: K =:= String): Table = {

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
        ) =
          if ((previousPaths == List(FieldName.Root) && !isNode(currentNode)) || previousNode.exists(isNode)) {
            go(left, previousPaths, Some(currentNode), descriptionsUsedAlready) ++
              go(right, previousPaths, Some(currentNode), descriptionsUsedAlready)
          } else {
            val tableWithAllDescriptions =
              (go(left, previousPaths, Some(currentNode), descriptionsUsedAlready) ++
                go(right, previousPaths, Some(currentNode), descriptionsUsedAlready))

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

            // If left has nested in it, then the parent has to have key in it.
            // or if right has nested in it, then the parent has to have key in it.
            // And if parent has key in it, then remove the key from the non nested.
            // If both left and right are nested, happy to keep the parent key as it is.

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
            ).liftToTable
          }

        docs match {
          case ConfigDocs.Leaf(sources, descriptions, _) =>
            val bla =
              descriptionsUsedAlready match {
                case Some(value) =>
                  descriptions.filter({
                    case ConfigDocs.Raw(_)                                              => true
                    case ConfigDocs.NestedDesc(path, _) if FieldName.Key(path) == value => false
                    case ConfigDocs.NestedDesc(_, _)                                    => true
                  })
                case None => descriptions
              }

            TableRow(previousPaths, Some(Table.Format.Primitive), bla, None, sources.map(_.name)).liftToTable

          case c @ ConfigDocs.Nested(path, docs) =>
            val result =
              go(
                docs,
                previousPaths :+ Table.FieldName.Key(path),
                Some(c),
                descriptionsUsedAlready = descriptionsUsedAlready
              )

            val resultPath =
              if (result.parentPaths == previousPaths :+ Table.FieldName.Key(path)) Nil
              else
                previousPaths :+ Table.FieldName.Key(path)

            // Peak ahead, and only if the next one is nested, keep nesting
            if (docs.is {
                  case ConfigDocs.Nested(_, _) => true
                }(false)) {
              TableRow(
                previousPaths :+ Table.FieldName.Key(path),
                Some(Table.Format.Nested),
                Nil,
                Some(result.copy(parentPaths = resultPath)),
                Set.empty
              ).liftToTable
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

      go(self, List(FieldName.Root), None, None)
    }
  }

  object ConfigDocs {
    sealed trait Description {
      override def toString: String =
        this match {
          case ConfigDocs.Raw(value)                  => value
          case ConfigDocs.NestedDesc(_, descriptions) => descriptions.toString
        }
    }

    case class Raw(value: String)                             extends Description
    case class NestedDesc(path: K, descriptions: Description) extends Description

    def nestedDes(path: K, description: Description): Description =
      NestedDesc(path, description)

    def raw(value: String): Description =
      Raw(value)

    def findByPath(description: List[Description], path: FieldName): List[Description] =
      description
        .flatMap(
          desc =>
            desc match {
              case ConfigDocs.Raw(_)                                             => Nil
              case ConfigDocs.NestedDesc(p, value) if (FieldName.Key(p) == path) => List(value)
              case ConfigDocs.NestedDesc(_, _)                                   => Nil
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
   * @param parentPaths: A table can be associated with a path towards it, which becomes its identity
   * @param rows: A table consist of multiple rows, where each row is a field and it's details.
   */
  case class Table(parentPaths: List[FieldName], rows: List[TableRow]) { self =>
    def setFormatGlobally(format: Format): Table =
      Table(parentPaths, rows.map(_.copy(format = Some(format))))

    def ++(that: Table): Table =
      Table(parentPaths, rows ++ that.rows)

    def asMarkdownContent(implicit S: K =:= String): String = {
      val headingColumns =
        List(
          "FieldName",
          "Format",
          "Description",
          "Sources"
        )

      def go(table: Table, index: Option[Int]): List[String] = {
        val (contents, nestedTables): (List[List[String]], List[(Table, Int)]) =
          table.rows.zipWithIndex.foldLeft((List.empty[List[String]], List.empty[(Table, Int)])) {
            case ((contentList, nestedTableList), (row, subIndex)) =>
              val lastFieldName = getLastFieldName(row.previousPaths)
              val formatString  = row.format.map(_.asString).getOrElse("")

              val (name, format) = row.nested match {
                case Some(nestedTable) =>
                  val parentPathString = getPathString(nestedTable.parentPaths, index, Some(subIndex))
                  val getLinkFn        = (s: String) => getLink(s, parentPathString)

                  getLinkFn(lastFieldName) -> getLinkFn(formatString)

                case None =>
                  lastFieldName -> formatString
              }

              (List(name, format, row.description.mkString(", "), row.sources.mkString(", ")) :: contentList) ->
                (row.nested.map((_, subIndex)).toList ++ nestedTableList)
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
                List(s"### ${getPathString(table._1.parentPaths, index, Some(table._2))}"),
                System.lineSeparator()
              ) :: go(table._1, Some(table._2))
          )
      }

      mkStringAndWrapWith(
        s"## Configuration Details" :: (System.lineSeparator() :: go(self, None)),
        System.lineSeparator()
      )
    }

    private def padToEmpty(string: String, size: Int): String = {
      val maxSize = Math.max(string.length, size)
      string.padTo(maxSize, ' ')
    }

    private def getPathString(paths: List[FieldName], parentIndex: Option[Int], subIndex: Option[Int])(
      implicit S: K =:= String
    ): String = {
      val optString: Option[Int] => String = _.fold("")("_" + _)

      paths.map(_.asString).mkString(".") + optString(parentIndex) + optString(subIndex)
    }

    private def wrapWith(input: String, str: String): String =
      str ++ input ++ str

    private def mkStringAndWrapWith(input: List[String], str: String): String =
      wrapWith(input.mkString(str), str)

    private def getLastFieldName(fieldNames: List[FieldName])(implicit S: K =:= String): String =
      fieldNames.lastOption.fold("")(last => if (last == FieldName.Root) "" else last.asString)

    private def getLink(name: String, link: String): String =
      if (name.isEmpty || link.isEmpty) "" else s"[${name}](#${getMarkdownLinkString(link)})"

    private def getMarkdownLinkString(s: String) =
      s.replace(".", "").toLowerCase

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

    case class TableRow(
      previousPaths: List[FieldName],
      format: Option[Format],
      description: List[ConfigDocs.Description],
      nested: Option[Table],
      sources: Set[String]
    ) {
      // A single row can be turned to a table
      def liftToTable =
        Table(List(FieldName.Root), List(this))
    }

    sealed trait Format { self =>
      def asString: String =
        self match {
          case Format.List                 => "list"
          case Format.Map                  => "map"
          case Format.Primitive            => "primitive"
          case Format.Nested               => "nested"
          case Format.AnyOneOf             => "any-one-of"
          case Format.AllOf                => "all-of"
          case Format.Of(format1, format2) => format1.asString + " >>> " + format2.asString
        }
    }

    object Format {
      case object List                               extends Format
      case object Map                                extends Format
      case object Primitive                          extends Format
      case object Nested                             extends Format
      case object AnyOneOf                           extends Format
      case object AllOf                              extends Format
      case class Of(format: Format, format2: Format) extends Format
    }

    sealed trait FieldName {
      def asString(implicit S: K =:= String): String =
        this match {
          case FieldName.Key(k) => S.apply(k)
          case FieldName.Root   => "root"
          case FieldName.Blank  => ""
        }
    }

    object FieldName {
      case object Root     extends FieldName
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
      latestPath: Option[K],
      isNested: Boolean
    ): ConfigDocs =
      config match {
        case Source(source, _) =>
          DocsLeaf((source.names ++ sources), descriptions, None)

        case Default(c, _) =>
          loop(sources, descriptions, c, latestPath, isNested = false)

        case cd: DynamicMap[_] =>
          ConfigDocs.DynamicMap(
            loop((cd.source.names ++ sources), descriptions, cd.config, None, isNested = false)
          )

        case Optional(c) =>
          loop(sources, descriptions, c, latestPath, isNested = false)

        case Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop((source.names ++ sources), descriptions, c, latestPath, isNested = false)
          )

        case Describe(c, desc) =>
          val descri: ConfigDocs.Description =
            if (isNested)
              latestPath.fold(ConfigDocs.raw(desc))(path => ConfigDocs.nestedDes(path, ConfigDocs.raw(desc)))
            else ConfigDocs.raw(desc)

          loop(sources, descri :: descriptions, c, latestPath, isNested)

        case Nested(path, c) =>
          ConfigDocs.Nested(path, loop(sources, descriptions, c, Some(path), isNested = true))

        case XmapEither(c, _, _) =>
          loop(sources, descriptions, c, latestPath, isNested = false)

        case Zip(left, right) =>
          ConfigDocs.Zip(
            loop(sources, descriptions, left, latestPath, isNested = false),
            loop(sources, descriptions, right, latestPath, isNested = false)
          )

        case OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath, isNested = false),
            loop(sources, descriptions, right, latestPath, isNested = false)
          )

        case OrElse(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath, isNested = false),
            loop(sources, descriptions, right, latestPath, isNested = false)
          )
      }

    loop(Set.empty, Nil, config, None, isNested = false)
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
                case _ => ConfigDocs.Sequence(schema, loop(tree, schema, keys) :: values)
              }
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
