package zio.config

trait ConfigDocsModule extends WriteModule {

  import ConfigDescriptorAdt._
  import Table._

  sealed trait ConfigDocs {
    self =>
    def toTable(previousPath: Option[FieldName]): Table = self match {
      case ConfigDocs.Leaf(sources, descriptions, _) =>
        TableRow(previousPath, Some(Table.Format.Primitive), descriptions, None, sources.map(_.name))
          .toTable(None)

      case ConfigDocs.Nested(path, docs) =>
        docs.toTable(Some(Table.FieldName.Raw(path)))

      case ConfigDocs.Zip(left, right) =>
        previousPath match {
          case Some(value) =>
            TableRow(
              Some(value),
              Some(Format.Nested),
              Nil,
              Some((left.toTable(None) ++ right.toTable(None)).copy(label = previousPath)),
              Set.empty
            ).toTable(None)

          case None =>
            left.toTable(None) ++ right.toTable(None)
        }

      case ConfigDocs.OrElse(leftDocs, rightDocs) =>
        TableRow(
          Some(FieldName.AnyOneOf),
          Some(Format.AnyOneOf),
          Nil,
          Some(leftDocs.toTable(previousPath) ++ rightDocs.toTable(previousPath)),
          Set.empty
        ).toTable(Some(FieldName.AnyOneOf))

      case ConfigDocs.Sequence(schemaDocs, _) =>
        schemaDocs.toTable(previousPath).setFormat(Format.List)

      case ConfigDocs.DynamicMap(schemaDocs, _) =>
        schemaDocs.toTable(previousPath).setFormat(Format.Map)
    }
  }

  object ConfigDocs {

    case class Leaf(sources: Set[ConfigSourceName], descriptions: List[String], value: Option[V] = None)
        extends ConfigDocs

    case class Nested(path: K, docs: ConfigDocs) extends ConfigDocs

    case class Zip(left: ConfigDocs, right: ConfigDocs) extends ConfigDocs

    case class OrElse(leftDocs: ConfigDocs, rightDocs: ConfigDocs) extends ConfigDocs

    case class Sequence(schemaDocs: ConfigDocs, valueDocs: List[ConfigDocs] = List.empty) extends ConfigDocs

    case class DynamicMap(
      schemaDocs: ConfigDocs,
      valueDocs: Map[K, ConfigDocs] = Map.empty[K, ConfigDocs]
    ) extends ConfigDocs

  }

  import Table.TableRow
  import Table.Format

  case class Table(label: Option[FieldName], list: List[TableRow]) {
    self =>
    def setName(name: FieldName): Table = Table(label, list.map(_.copy(name = Some(name))))

    def setFormat(format: Format): Table = Table(label, list.map(_.copy(format = Some(format))))

    def ++(table: Table): Table =
      Table(label, list ++ table.list)

    def asMarkdownContent: String = {
      def getHeadingLabels[A](label: A): String =
        s"[${label}](#${label})"

      def size(list: List[Int]): Int =
        if (list.isEmpty) 0 else Math.max(10, list.max)

      def go(table: Table): List[String] = {
        val maxNameLength: Int = Math.max(
          10,
          size(
            table.list.flatMap(
              table =>
                table.nested match {
                  case Some(value) =>
                    value.label
                      .map(
                        t =>
                          table.name
                            .fold(getHeadingLabels(t).length)(
                              name => Math.max(getHeadingLabels(t).length, name.toString.length)
                            )
                      )
                      .toList
                  case None => table.name.map(_.toString.length).toList
                }
            )
          )
        )

        val maxFormatLength =
          size(
            table.list.flatMap(
              table =>
                table.nested match {
                  case Some(_) =>
                    table.format.map(format => getHeadingLabels(format.toString).length).toList
                  case None =>
                    table.format.map(format => format.toString.length).toList
                }
            )
          )

        val maxDescriptionLength: Int =
          size(table.list.flatMap(_.description.map(_.toString.length)))

        val maxSourcesLength: Int =
          size(table.list.flatMap(_.sources.map(_.toString.length)))

        val heading: String =
          " | " ++
            List(
              "FieldName".padTo(maxNameLength, ' '),
              "Format".padTo(maxFormatLength, ' '),
              "Description".padTo(maxDescriptionLength, ' '),
              "Sources".padTo(maxSourcesLength, ' ')
            ).mkString(" | ") ++ " | "

        val secondRow: String =
          " | " ++ List(maxNameLength, maxFormatLength, maxDescriptionLength, maxSourcesLength)
            .map(
              length => "----------".padTo(length, ' ')
            )
            .mkString(" | ") + " | "

        val (contents, nestedTables): (List[String], List[Table]) = {
          table.list.foldLeft((List.empty[String], List.empty[Table])) {
            case ((contentList, tableList), t) => {
              (
                " | " ++ {

                  val (name, format) = t.nested match {
                    case Some(value) =>
                      value.label match {
                        case Some(value) =>
                          (
                            getString(Some(s"[${value}](#${value})"), maxNameLength),
                            getString(Some(getHeadingLabels(Format.Nested.toString)), maxFormatLength)
                          )
                        case None =>
                          (getString(t.name, maxNameLength), getString(t.format.map(_.toString), maxFormatLength))
                      }

                    case None =>
                      (getString(t.name, maxNameLength), getString(t.format.map(_.toString), maxFormatLength))
                  }

                  List(
                    name,
                    format,
                    t.description.mkString(", ").padTo(maxDescriptionLength, ' '),
                    t.sources.mkString(", ").padTo(maxSourcesLength, ' ')
                  ).mkString(" | ")

                } + " | " :: contentList,
                t.nested.toList ++ tableList
              )
            }
          }
        }

        (List(heading, secondRow) ++ contents).mkString("\n") :: nestedTables.flatMap(
          t => t.label.toList.map(t => s"### ${t}") ++ go(t)
        )
      }

      s"## Configuration Details" ++ "\n \n" ++ go(self).mkString("\n\n")
    }

    def getString[A](option: Option[A], size: Int): String =
      option.map(t => t.toString.padTo(Math.max(t.toString.length, size), ' ')).getOrElse(" ".padTo(size, ' '))

  }

  object Table {

    case class TableRow(
      name: Option[FieldName],
      format: Option[Format],
      description: List[String],
      nested: Option[Table],
      sources: Set[String]
    ) {
      def toTable(label: Option[FieldName]) =
        Table(label, List(this))
    }

    sealed trait Format

    object Format {

      case object List extends Format

      case object Map extends Format

      case object Primitive extends Format

      case object Nested extends Format

      case object AnyOneOf extends Format

    }

    abstract sealed class FieldName(val name: String) {
      override def toString: String = this match {
        case FieldName.Raw(k)   => k.toString
        case FieldName.AnyOneOf => "<Not Applicable>"
      }
    }

    object FieldName {
      case class Raw[K](k: K) extends FieldName(k.toString)
      case object AnyOneOf    extends FieldName("any-one-of")
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
