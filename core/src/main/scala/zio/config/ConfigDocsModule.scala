package zio.config

trait ConfigDocsModule extends WriteModule {

  import ConfigDescriptorAdt._
  import Table._

  sealed trait ConfigDocs { self =>
    def toTable: Table = {
      def go(docs: ConfigDocs, previousPaths: Set[FieldName]): Table =
        docs match {
          case ConfigDocs.Leaf(sources, descriptions, _) =>
            TableRow(previousPaths, Some(Table.Format.Primitive), descriptions, None, sources.map(_.name)).liftToTable

          case ConfigDocs.Nested(path, docs) =>
            go(docs, (previousPaths.toList :+ Table.FieldName.Key(path)).toSet)

          case ConfigDocs.Zip(left, right) =>
            if (previousPaths != Set(FieldName.Root))
              TableRow(
                previousPaths,
                Some(Format.AllOf),
                Nil,
                Some((go(left, previousPaths) ++ go(right, previousPaths)).copy(parentPaths = previousPaths)),
                Set.empty
              ).liftToTable
            else
              go(left, previousPaths) ++ go(right, previousPaths)

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
            go(schemaDocs, previousPaths).setFormat(Format.List)

          case ConfigDocs.DynamicMap(schemaDocs, _) =>
            go(schemaDocs, previousPaths).setFormat(Format.Map)
        }

      go(self, Set(FieldName.Root))

    }
  }

  object ConfigDocs {

    case class Leaf(
      sources: Set[ConfigSourceName],
      descriptions: List[String],
      value: Option[V] = None
    ) extends ConfigDocs
    case class Nested(path: K, docs: ConfigDocs)                                          extends ConfigDocs
    case class Zip(left: ConfigDocs, right: ConfigDocs)                                   extends ConfigDocs
    case class OrElse(leftDocs: ConfigDocs, rightDocs: ConfigDocs)                        extends ConfigDocs
    case class Sequence(schemaDocs: ConfigDocs, valueDocs: List[ConfigDocs] = List.empty) extends ConfigDocs
    case class DynamicMap(
      schemaDocs: ConfigDocs,
      valueDocs: Map[K, ConfigDocs] = Map.empty[K, ConfigDocs]
    ) extends ConfigDocs

  }

  import Table.TableRow
  import Table.Format

  case class Label(index: Int, name: Option[FieldName]) {
    def asString(implicit S: K =:= String): String = name match {
      case Some(value) => getString(index.toString, value.asString)
      case None        => index.toString
    }

    private def getString(index: String, name: String): String =
      List(index, name).mkString("_")
  }

  case class Table(parentPaths: Set[FieldName], rows: List[TableRow]) { self =>
    def setFormat(format: Format): Table = Table(parentPaths, rows.map(_.copy(format = Some(format))))

    def ++(table: Table): Table =
      Table(parentPaths, rows ++ table.rows)

    def asMarkdownContent(implicit S: K =:= String): String = {
      def getHeadingLabels[A](label: A): String =
        s"[${label}](#${label})"

      def getMaxSize(list: List[Int]): Int =
        if (list.isEmpty) 10 else Math.max(10, list.max)

      def sizesOfNamesBasedOnFormat(table: Table): List[Int] =
        table.rows.map(
          row =>
            row.format match {
              case Some(format) =>
                format match {
                  case Format.List      => row.previousPaths.map(_.asString).mkString(".").length
                  case Format.Map       => row.previousPaths.map(_.asString).mkString(".").length
                  case Format.Primitive => row.previousPaths.map(_.asString).mkString(".").length
                  case Format.AllOf =>
                    row.nested match {
                      case Some(_) => 30
                      case None    => row.previousPaths.map(_.asString).mkString(".").length
                    }
                  case Format.Nested =>
                    row.nested match {
                      case Some(_) => 30
                      case None    => row.previousPaths.map(_.asString).mkString(".").length
                    }
                  case Format.AnyOneOf =>
                    row.nested match {
                      case Some(_) => 30
                      case None    => row.previousPaths.map(_.asString).mkString(".").length
                    }
                }

              case None => row.previousPaths.map(_.asString).mkString(".").length
            }
        )

      def sizesOfFormats(table: Table): List[Int] =
        table.rows.flatMap(
          table =>
            table.format match {
              case Some(value) =>
                value match {
                  case Table.Format.Nested =>
                    table.format.map(format => getHeadingLabels(format.asString).length).toList
                  case Table.Format.AnyOneOf =>
                    table.format.map(format => getHeadingLabels(format.asString).length).toList
                  case Table.Format.AllOf =>
                    table.format.map(format => getHeadingLabels(format.asString).length).toList
                  case Table.Format.List =>
                    table.format.map(_.asString.length).toList
                  case Table.Format.Map =>
                    table.format.map(_.asString.length).toList
                  case Table.Format.Primitive =>
                    table.format.map(_.asString.length).toList
                }
              case None =>
                table.format.map(format => format.asString.length).toList
            }
        )

      def getSizesOfDescriptions(table: Table): List[Int] =
        table.rows.flatMap(_.description.map(_.length))

      def getSizesOfSources(table: Table): List[Int] =
        table.rows.flatMap(_.sources.map(_.length))

      def go(table: Table): List[String] = {
        val maxNameLength: Int =
          getMaxSize(sizesOfNamesBasedOnFormat(table))

        val maxFormatLength =
          getMaxSize(sizesOfFormats(table))

        val maxDescriptionLength: Int =
          getMaxSize(getSizesOfDescriptions(table))

        val maxSourcesLength: Int =
          getMaxSize(getSizesOfSources(table))

        val headingColumns =
          List(
            "FieldName",
            "Format",
            "Description",
            "Sources"
          )

        val headingSizes =
          List(
            maxNameLength,
            maxFormatLength,
            maxDescriptionLength,
            maxSourcesLength
          )

        val heading: String =
          mkStringAndWrapWith(
            headingColumns
              .zip(headingSizes)
              .map({
                case (name, int) => padToEmpty(name, int)
              }),
            "|"
          )

        val secondRow: String =
          mkStringAndWrapWith(headingSizes.map(padToEmpty("----------", _)), "|")

        val (contents, nestedTables): (List[String], List[Table]) = {
          def getString[A](option: Set[A], size: Int)(f: A => String): String =
            padToEmpty(option.map(f).mkString("."), size)

          def getLastFieldName(fieldNames: Set[FieldName]): String = {
            val s = fieldNames.lastOption.fold("")(_.asString)
            println(s)
            s
          }

          table.rows.foldLeft((List.empty[String], List.empty[Table])) {
            case ((contentList, nestedTableList), row) => {
              val (name, format) = row.nested match {
                case Some(nestedTable) =>
                  (
                    s"[${getLastFieldName(row.previousPaths)}](#${nestedTable.parentPaths.map(_.asString).mkString(".")})", {
                      val nameOfFormat = getString(row.format.toList.toSet, 0)(_.asString)
                      padToEmpty(
                        s"[${nameOfFormat}](#${nestedTable.parentPaths.map(_.asString).mkString(".")})",
                        maxFormatLength
                      )
                    }
                  )

                case None =>
                  (
                    getLastFieldName(row.previousPaths),
                    getString(row.format.toList.toSet, maxFormatLength)(_.asString)
                  )
              }
              (
                mkStringAndWrapWith(
                  List(
                    name,
                    format,
                    padToEmpty(row.description.mkString(", "), maxDescriptionLength),
                    padToEmpty(row.sources.mkString(", "), maxSourcesLength)
                  ),
                  "|"
                ) :: contentList,
                row.nested.toList ++ nestedTableList
              )
            }
          }
        }

        (List(heading, secondRow) ++ contents).mkString("\n") :: nestedTables.flatMap(
          table => s"### ${table.parentPaths.map(_.asString).mkString(".")}" +: go(table)
        )
      }

      s"## Configuration Details" ++ "\n \n" ++ go(self).mkString("\n\n")
    }

    def padToEmpty(str: String, size: Int): String = {
      val maxSize = Math.max(str.length, size)
      str.padTo(maxSize, ' ')
    }

    def wrapWith(input: String, str: String): String =
      str ++ input ++ str

    def mkStringAndWrapWith(input: List[String], str: String): String =
      wrapWith(input.mkString(str), str)

  }

  object Table {

    case class TableRow(
      previousPaths: Set[FieldName],
      format: Option[Format],
      description: List[String],
      nested: Option[Table],
      sources: Set[String]
    ) {
      def liftToTable =
        Table(Set(FieldName.Root), List(this))
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
          case FieldName.Key(k)   => S.apply(k)
          case FieldName.AnyOneOf => ""
          case FieldName.Root     => "root"
        }
    }

    object FieldName {
      case object Root     extends FieldName
      case class Key(k: K) extends FieldName
      case object AnyOneOf extends FieldName
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
