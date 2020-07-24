package zio.config

trait ConfigDocsModule extends WriteModule {

  import ConfigDescriptorAdt._
  import Table._

  sealed trait ConfigDocs { self =>

    /**
     * Convert ConfigDocs to `Table`, which is a light-weight and flattened, yet
     * recursive structure, that can then be easily turned into human readable formats such as markdown, table, html etc
     */
    def toTable: Table = {
      def go(docs: ConfigDocs, previousPaths: List[FieldName]): Table =
        docs match {
          case ConfigDocs.Leaf(sources, descriptions, _) =>
            TableRow(previousPaths, Some(Table.Format.Primitive), descriptions, None, sources.map(_.name)).liftToTable

          case ConfigDocs.Nested(path, docs) =>
            go(docs, (previousPaths :+ Table.FieldName.Key(path)))

          case ConfigDocs.Zip(left, right) =>
            if (previousPaths == List(FieldName.Root))
              go(left, previousPaths) ++ go(right, previousPaths)
            else
              TableRow(
                previousPaths,
                Some(Format.AllOf),
                Nil,
                Some((go(left, previousPaths) ++ go(right, previousPaths)).copy(parentPaths = previousPaths)),
                Set.empty
              ).liftToTable

          case ConfigDocs.OrElse(leftDocs, rightDocs) =>
            TableRow(
              previousPaths,
              Some(Format.AnyOneOf),
              Nil,
              Some(
                (go(leftDocs, previousPaths) ++ go(rightDocs, previousPaths))
                  .copy(parentPaths = previousPaths)
              ),
              Set.empty
            ).liftToTable

          case ConfigDocs.Sequence(schemaDocs, _) =>
            go(schemaDocs, previousPaths).setFormatGlobally(Format.List)

          case ConfigDocs.DynamicMap(schemaDocs, _) =>
            go(schemaDocs, previousPaths).setFormatGlobally(Format.Map)
        }

      go(self, List(FieldName.Root))
    }
  }

  object ConfigDocs {
    case class Leaf(sources: Set[ConfigSourceName], descriptions: List[String], value: Option[V] = None)
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
                  val getLinkFn        = getLink(_, parentPathString)

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
      if (name.isEmpty || link.isEmpty) "" else s"[${name}](#${link})"

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
      description: List[String],
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
          case Format.List      => "list"
          case Format.Map       => "map"
          case Format.Primitive => "primitive"
          case Format.Nested    => "nested"
          case Format.AnyOneOf  => "any-one-of"
          case Format.AllOf     => "all-of"
        }
    }

    object Format {
      case object List      extends Format
      case object Map       extends Format
      case object Primitive extends Format
      case object Nested    extends Format
      case object AnyOneOf  extends Format
      case object AllOf     extends Format
    }

    sealed trait FieldName {
      def asString(implicit S: K =:= String): String =
        this match {
          case FieldName.Key(k) => S.apply(k)
          case FieldName.Root   => "root"
        }
    }

    object FieldName {
      case object Root     extends FieldName
      case class Key(k: K) extends FieldName
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
      descriptions: List[String],
      config: ConfigDescriptor[B],
      latestPath: Option[K]
    ): ConfigDocs =
      config match {
        case Source(source, _) =>
          DocsLeaf((source.names ++ sources), descriptions, None)

        case Default(c, _) =>
          loop(sources, descriptions, c, latestPath)

        case cd: DynamicMap[_] =>
          ConfigDocs.DynamicMap(
            loop((cd.source.names ++ sources), descriptions, cd.config, None)
          )

        case Optional(c) =>
          loop(sources, descriptions, c, latestPath)

        case Sequence(source, c) =>
          ConfigDocs.Sequence(
            loop((source.names ++ sources), descriptions, c, latestPath)
          )

        case Describe(c, desc) =>
          loop(sources, desc :: descriptions, c, latestPath)

        case Nested(path, c) =>
          ConfigDocs.Nested(path, loop(sources, descriptions, c, Some(path)))

        case XmapEither(c, _, _) =>
          loop(sources, descriptions, c, latestPath)

        case Zip(left, right) =>
          ConfigDocs.Zip(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case OrElseEither(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
          )

        case OrElse(left, right) =>
          ConfigDocs.OrElse(
            loop(sources, descriptions, left, latestPath),
            loop(sources, descriptions, right, latestPath)
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
                case _ => ConfigDocs.Sequence(schema, loop(tree, schema, keys) :: values)
              }
          }

        loop(tree, generateDocs(config), List.empty)
      })
}
