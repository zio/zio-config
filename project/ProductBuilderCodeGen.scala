import sbt._
import java.nio.file._
import org.scalafmt.interfaces.Scalafmt

object ProductBuilderCodeGen {
  import scala.io.Source

  // shabby-code: ON

  ////

  private val count = 22

  def productBuilderCodes: List[String] =
    (2 until count)
      .flatMap(productBuilderCode)
      .toList ++
      List.fill(count - 2)("}")

  val allLettersExceptF = "abcdeghijklmnopqrstuvwxyz"

  private def productBuilderCode(n: Int): List[String] = {
    def letter(i: Int) =
      allLettersExceptF(i).toString

    val l0  = letter(n + 0) // eg "d"
    val L0  = l0.toUpperCase
    val l1  = letter(n + 1) // eg "e"
    val L1  = l1.toUpperCase
    val ll1 = l1 + l1 // eg "ee"

    val letters  = (0 to n).map(letter)
    val letters2 = letters.map(c => s"$c$c")

    val cL0        = letters.mkString(", ").toUpperCase // eg "A, B, C, D"
    val cll0       = letters2.mkString(", ") // eg "aa, bb, cc, dd"
    val zipped     = letters.mkString(" zip ")
    val cll0Tupled = "(" * n + "aa, " + letters2.drop(1).mkString("", "), ", ")") // eg "(((aa, bb), cc), dd)"

    val part1 =
      s"""
         |  sealed abstract class ProductBuilder[$L0] {
         |    val $l0: ConfigDescriptor[$L0]
         |    def apply[$L1](ff: ($cL0) => $L1, gg: $L1 => Option[($cL0)]): ConfigDescriptor[$L1] =
         |      ($zipped)
         |        .xmapEither[$L1] {
         |          case $cll0Tupled => Right(ff($cll0))
         |        }(
         |          liftWrite($l1 => gg($l1).map { case ($cll0) => $cll0Tupled })
         |      )""".stripMargin
    val part2 =
      s"""
         |    def |@|[$L1]($ll1: ConfigDescriptor[$L1]): ProductBuilder[$L1] =
         |      new ProductBuilder[$L1] {
         |        val $l1: ConfigDescriptor[$L1] = $ll1
         |      }""".stripMargin

    if (n == count - 1) List(part1) else List(part1, part2)
  }

  def readFile(filepath: File): List[String] = {
    val source = Source.fromFile(filepath.toString)
    try {
      source.getLines.toList
    } finally {
      source.close
    }
  }

  def replaceFileSection(
    filepath: File,
    marker: String,
    newContents: List[String],
    tempScalaFmtFile: File,
    scalaFmtPath: File
  ): Unit = {
    val scalafmt = Scalafmt.create(this.getClass.getClassLoader)

    val markerStart = s"/start/$marker/"
    val markerEnd   = s"/end/$marker/"
    val lines       = readFile(filepath)

    val markerStartLine = findLine(lines, markerStart, filepath)
    val markerEndLine   = findLine(lines, markerEnd, filepath)

    val beforeMarker = lines.takeWhile(_ != markerStartLine)
    val afterMarker  = lines.dropWhile(_ != markerEndLine).drop(1)

    val toWrite: List[String] = (beforeMarker :+ markerStartLine) ++ (newContents :+ markerEndLine) ++ afterMarker

    val result = IO.read(scalaFmtPath).replace("maxColumn = 120", "maxColumn = 12000")

    IO.write(tempScalaFmtFile, result)

    val formatted =
      scalafmt.format(tempScalaFmtFile.toPath, Paths.get("Main.scala"), toWrite.mkString("\n"))

    IO.write(filepath, formatted)
  }

  private def findLine(lines: List[String], marker: String, filepath: File): String =
    lines
      .find(_.contains(marker))
      .getOrElse(throw new RuntimeException(s"Cannot find marker $marker in file $filepath"))

  // shabby-code: OFF
}
