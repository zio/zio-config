package zio.config

import java.io.{ File, PrintWriter }

import ConfigDescriptor._
import zio.config.DocsSpecUtils._
import zio.test.Assertion._
import zio.test._

object DocsSpec
    extends BaseSpec(
      suite("partial products fail instead of returning none")(
        test(
          "Generate docs for simple config"
        ) {
          val caseClass2 =
            (string("region") |@| string("account_name"))(TestCase1.CaseClass2.apply, TestCase1.CaseClass2.unapply)

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("user") |@| nested("aws")(caseClass2) |@| list("locations")(string))(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val result = generateDocs(config from ConfigSource.fromMap(Map.empty, source = "system environment"))

          println(getDocs(result, None).getContent)

          val writer = new PrintWriter(new File("config.md"))

          writer.write(getDocs(result, None).getContent)
          writer.close()

          assert(Some(result))(
            equalTo(None: Option[ConfigDocs])
          )
        }
      )
    )

object DocsSpecUtils {
  object TestCase1 {
    case class CaseClass1(a: String, b: CaseClass2, c: List[String])
    case class CaseClass2(a: String, b: String)
  }

  def getDocs(config: ConfigDocs, previousPath: Option[K]): Table = config match {
    case ConfigDocs.Leaf(sources, descriptions, value) =>
      Table.fromRow(None, TableRow(previousPath, Some(Format.Primitive), descriptions, None, sources.map(_.name)))

    case ConfigDocs.Nested(path, docs) =>
      getDocs(docs, Some(path))

    case ConfigDocs.Zip(left, right) =>
      previousPath match {
        case Some(value) =>
          Table.fromRow(
            previousPath,
            TableRow(
              Some(value),
              Some(Format.Nested),
              Nil,
              Some((getDocs(left, None) ++ getDocs(right, None)).copy(label = previousPath)),
              Set.empty
            )
          )

        case None =>
          getDocs(left, None) ++ getDocs(right, None)
      }

    case ConfigDocs.OrElse(leftDocs, rightDocs)     => getDocs(leftDocs, previousPath) ++ getDocs(rightDocs, previousPath)
    case ConfigDocs.Sequence(schemaDocs, valueDocs) => getDocs(schemaDocs, previousPath).setFormat(Format.List)
    case ConfigDocs.DynamicMap(schemaDocs, valueDocs) =>
      getDocs(schemaDocs, previousPath).setFormat(Format.Map)
  }

  final case class TableRow(
    name: Option[String],
    format: Option[Format],
    description: List[String],
    nested: Option[Table],
    sources: Set[String]
  )

  sealed trait Format

  object Format {
    case object List      extends Format
    case object Map       extends Format
    case object Primitive extends Format
    case object Nested    extends Format
  }

  final case class Table(label: Option[K], list: List[TableRow]) { self =>
    def setName(name: String): Table     = Table(label, list.map(_.copy(name = Some(name))))
    def setFormat(format: Format): Table = Table(label, list.map(_.copy(format = Some(format))))

    def ++(table: Table): Table =
      Table(label, list ++ table.list)

    def getHeadingLabels(label: String): String =
      s"[${label}](#${label})"

    def size(list: List[Int]) =
      if (list.isEmpty) 0 else Math.max(10, list.max)

    def getContent: String = {
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
                            .fold(getHeadingLabels(t).length)(name => Math.max(getHeadingLabels(t).length, name.length))
                      )
                      .toList
                  case None => table.name.map(_.length).toList
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

    def getString(option: Option[String], size: Int): String =
      option.map(t => t.padTo(Math.max(t.length, size), ' ')).getOrElse(" ".padTo(size, ' '))

  }

  object Table {
    def fromRow(label: Option[String], tableRow: TableRow) =
      Table(label, List(tableRow))
  }
}
