package zio.config.yaml.generators

import zio.test.Gen

final case class WhiteSpacedClosingTag(
  openBracket: WhiteSpacedBracket.SlashedOpen,
  internalText: WhiteSpacedText,
  closedBracket: WhiteSpacedBracket.Closed
) {

  def print: String =
    Printer.print(openBracket.print, internalText.print, closedBracket.print)
}

object WhiteSpacedClosingTag {

  def gen(tagName: WhiteSpacedText): Gen[Any, WhiteSpacedClosingTag] =
    for {
      start <- WhiteSpacedBracket.slashedOpen
      stop  <- WhiteSpacedBracket.closed
    } yield WhiteSpacedClosingTag(start, tagName, stop)
}
