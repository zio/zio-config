package zio.config.tools

import java.io.{ BufferedWriter, File, FileWriter }

import scala.io.Source

// shabby-code: ON

object Codegen {
  def main(args: Array[String]): Unit =
    replaceFileSection(
      "zio-config/src/main/scala/zio/config/ProductBuilder.scala",
      "productbuilder",
      productBuilderCodes :+ ""
    )

  ////

  private val count = 22

  private def productBuilderCodes: List[String] =
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
         |    val $l0: Config[$L0]
         |    def apply[$L1](ff: ($cL0) => $L1, gg: $L1 => Option[($cL0)]): Config[$L1] =
         |      ($zipped).xmapEither[$L1] { case $cll0Tupled => Right(ff($cll0)) }(
         |        liftWrite($l1 => gg($l1).map { case ($cll0) => $cll0Tupled })
         |      )""".stripMargin
    val part2 =
      s"""
         |    def |@|[$L1]($ll1: Config[$L1]): ProductBuilder[$L1] =
         |      new ProductBuilder[$L1] {
         |        val $l1: Config[$L1] = $ll1
         |      }""".stripMargin

    if (n == count - 1) List(part1) else List(part1, part2)
  }

  def readFile(filepath: String): List[String] = {
    val source = Source.fromFile(filepath)
    try {
      source.getLines.toList
    } finally {
      source.close
    }
  }

  def replaceFileSection(filepath: String, marker: String, newContents: List[String]): Unit = {
    val markerStart = s"/start/$marker/"
    val markerEnd   = s"/end/$marker/"
    val lines       = readFile(filepath)

    val markerStartLine = findLine(lines, markerStart, filepath)
    val markerEndLine   = findLine(lines, markerEnd, filepath)

    val beforeMarker = lines.takeWhile(_ != markerStartLine)
    val afterMarker  = lines.dropWhile(_ != markerEndLine).drop(1)

    val toWrite: List[String] = (beforeMarker :+ markerStartLine) ++ (newContents :+ markerEndLine) ++ afterMarker

    val bw = new BufferedWriter(new FileWriter(new File(filepath)))
    toWrite.foreach { l =>
      bw.write(l)
      bw.newLine()
    }
    bw.close()
  }

  private def findLine(lines: List[String], marker: String, filepath: String): String =
    lines
      .find(_.contains(marker))
      .getOrElse(throw new RuntimeException(s"Cannot find marker $marker in file $filepath"))

}

// shabby-code: OFF
