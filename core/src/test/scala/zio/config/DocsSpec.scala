package zio.config

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
            (string("a1") |@| string("b1"))(TestCase1.CaseClass2.apply, TestCase1.CaseClass2.unapply)

          val config: ConfigDescriptor[TestCase1.CaseClass1] =
            (string("a") |@| caseClass2 |@| list("c")(string))(
              TestCase1.CaseClass1.apply,
              TestCase1.CaseClass1.unapply
            )

          val result = generateDocs(config from ConfigSource.fromMap(Map.empty))

          assert(Some(result))(
            equalTo(None: Option[ConfigDocs])
          )
        }
      )
    )

object DocsSpecUtils {
  // A data type that can be printed to html and markdown
  case class Segment(table: Table, segments: List[Segment])

  object TestCase1 {
    case class CaseClass1(a: String, b: CaseClass2, c: List[String])
    case class CaseClass2(a: String, b: String)
  }

  def getDocs(config: ConfigDocs): Table = config match {
    case ConfigDocs.Leaf(sources, descriptions, value) =>
      Table(List(TableRow(None, Some(Format.Primitive), descriptions, None, sources.map(_.name))))
    case ConfigDocs.Nested(path, docs) => {
      val existingDoc  = getDocs(docs)
      val existingPath = existingDoc.list.flatMap(_.name.toList)

      if (existingPath.nonEmpty) {
        Table(existingDoc.list.map(t => t.copy(link = t.name), ))
      }

    }

    getDocs(docs).setName(path)
  case ConfigDocs.Zip(left, right)                  => getDocs(left) ++ getDocs(right)
    case ConfigDocs.OrElse(leftDocs, rightDocs)     => getDocs(leftDocs) ++ getDocs(rightDocs)
    case ConfigDocs.Sequence(schemaDocs, valueDocs) => getDocs(schemaDocs).setFormat(Format.List)
    case ConfigDocs.DynamicMap(schemaDocs, valueDocs) =>
      getDocs(schemaDocs).setFormat(Format.Map)
  }

  final case class TableRow(
    name: Option[String],
    format: Option[Format],
    description: List[String],
    link: Option[String],
    sources: Set[String]
  )

  sealed trait Format

  object Format {
    case object List      extends Format
    case object Map       extends Format
    case object Primitive extends Format
  }

  final case class Table(parentNode: Option[String], list: List[TableRow]) {
    def setName(name: String): Table     = Table(parentNode, list.map(_.copy(name = Some(name))))
    def setFormat(format: Format): Table = Table(parentNode, list.map(_.copy(format = Some(format))))
    def setLink(link: String): Table     = Table(parentNode, list.map(_.copy(link = Some(link))))

    def ++(table: Table): Table =
      Table(parentNode, list ++ table.list)

    val maxNameLength: Int   = Math.max(10, size(list.flatMap(_.name.map(_.length).toList)))
    val maxFormatLength: Int = Math.max(10, size(list.flatMap(_.format.map(_.toString.length).toList)))
    val maxDescriptionLength: Int =
      Math.max(10, size(list.flatMap(_.description.map(_.mkString(",").length))))
    val maxLinkLength: Int    = Math.max(10, size(list.flatMap(_.link.map(_.length).toList)))
    val maxSourcesLength: Int = Math.max(10, size(list.flatMap(_.sources.map(_.mkString(", ").length))))

    def size(list: List[Int]) =
      if (list.isEmpty) 0 else list.max

    val heading: String =
      "| " ++
        List(
          "Name".padTo(maxNameLength, ' '),
          "Format".padTo(maxFormatLength, ' '),
          "Description".padTo(maxDescriptionLength, ' '),
          "Link".padTo(maxLinkLength, ' '),
          "Sources".padTo(maxSourcesLength, ' ')
        ).mkString("| ") ++ "| "

    val secondRow: String =
      "| " ++ List(maxNameLength, maxFormatLength, maxDescriptionLength, maxLinkLength, maxSourcesLength)
        .map(
          length => "----------".padTo(length, ' ')
        )
        .mkString("| ") ++ "| "

    val content = list.map(
      t =>
        "| " ++
          List(
            getString(t.name, maxNameLength),
            getString(t.format.map(_.toString), maxFormatLength),
            t.description.mkString(", ").padTo(maxDescriptionLength, ' '),
            getString(t.link, maxLinkLength),
            t.sources.mkString(", ").padTo(maxSourcesLength, ' ')
          ).mkString("| ") ++ "| "
    )

    val asString = (heading :: secondRow :: content).mkString("\n")

    def getString(option: Option[String], size: Int): String =
      option.map(_.padTo(size, ' ')).getOrElse(" ".padTo(size, ' '))

  }
}
