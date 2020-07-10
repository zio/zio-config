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
      Table(None, List(TableRow(previousPath, Some(Format.PrimitiveFormat), descriptions, None, sources.map(_.name))))

    case ConfigDocs.Nested(path, docs) =>
      getDocs(docs, Some(path))

    case ConfigDocs.Zip(left, right) =>
      previousPath match {
        case Some(value) =>
          Table(
            previousPath,
            List(
              TableRow(
                Some(value),
                Some(Format.Nested),
                Nil,
                Some((getDocs(left, None) ++ getDocs(right, None)).copy(label = previousPath)),
                Set.empty
              )
            )
          )

        case None =>
          getDocs(left, None) ++ getDocs(right, None)
      }

    case ConfigDocs.OrElse(leftDocs, rightDocs)     => getDocs(leftDocs, previousPath) ++ getDocs(rightDocs, previousPath)
    case ConfigDocs.Sequence(schemaDocs, valueDocs) => getDocs(schemaDocs, previousPath).setFormat(Format.ListFormat)
    case ConfigDocs.DynamicMap(schemaDocs, valueDocs) =>
      getDocs(schemaDocs, previousPath).setFormat(Format.MapFormat)
  }

  sealed trait Markdown

  final case class TableRow(
    name: Option[String],
    format: Option[Format],
    description: List[String],
    link: Option[Table],
    sources: Set[String]
  )

  sealed trait Format

  object Format {
    case object ListFormat      extends Format
    case object MapFormat       extends Format
    case object PrimitiveFormat extends Format
    case object Nested          extends Format
  }

  final case class Table(label: Option[K], list: List[TableRow]) { self =>
    def setName(name: String): Table     = Table(label, list.map(_.copy(name = Some(name))))
    def setFormat(format: Format): Table = Table(label, list.map(_.copy(format = Some(format))))

    def ++(table: Table): Table =
      Table(label, list ++ table.list)

    val maxNameLength: Int   = Math.max(10, size(list.flatMap(_.name.map(_.length).toList)))
    val maxFormatLength: Int = Math.max(10, size(list.flatMap(_.format.map(_.toString.length).toList)))
    val maxDescriptionLength: Int =
      Math.max(10, size(list.flatMap(_.description.map(_.mkString(",").length))))
    val maxLinkLength: Int    = 30
    val maxSourcesLength: Int = Math.max(10, size(list.flatMap(_.sources.map(_.mkString(", ").length))))

    def size(list: List[Int]) =
      if (list.isEmpty) 0 else list.max

    def getContent: String = {
      def go(table: Table): List[String] = {
        val heading: String =
          " | " ++
            List(
              "FieldName".padTo(maxNameLength, ' '),
              "Format".padTo(maxFormatLength, ' '),
              "Description".padTo(maxDescriptionLength, ' '),
              "Link".padTo(maxLinkLength, ' '),
              "Sources".padTo(maxSourcesLength, ' ')
            ).mkString(" | ") ++ " | "

        val secondRow: String =
          " | " ++ List(maxNameLength, maxFormatLength, maxDescriptionLength, maxLinkLength, maxSourcesLength)
            .map(
              length => "----------".padTo(length, ' ')
            )
            .mkString(" | ") + " | "

        val xx: (List[String], List[Table]) = {
          table.list.foldLeft((List.empty[String], List.empty[Table])) {
            case ((contentList, tableList), t) => {
              (
                " | " ++
                  List(
                    getString(t.name, maxNameLength),
                    getString(t.format.map(_.toString), maxFormatLength),
                    t.description.mkString(", ").padTo(maxDescriptionLength, ' '),
                    getString(t.link.flatMap(t => t.label.map(k => s"[${k}](#${k})")), maxLinkLength),
                    t.sources.mkString(", ").padTo(maxSourcesLength, ' ')
                  ).mkString(" | ") + " | " :: contentList,
                t.link.toList ++ tableList
              )
            }
          }
        }

        val asString = (List(heading, secondRow) ++ xx._1).mkString("\n")

        asString :: xx._2.flatMap(t => t.label.toList.map(t => s"### ${t}") ++ go(t))
      }

      go(self).mkString("\n \n")
    }

    def getString(option: Option[String], size: Int): String =
      option.map(t => t.padTo(Math.max(t.length, size), ' ')).getOrElse(" ".padTo(size, ' '))

  }
}
