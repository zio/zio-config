package zio.config.yaml.generators

import zio.test.Gen

final case class WhiteSpacedOpenTag(
  whiteSpacedOpenBracket: WhiteSpacedBracket.Open,
  text: WhiteSpacedText,
  attributes: WhiteSpacedAttributes,
  whiteSpacedClosingBracket: WhiteSpacedBracket.Closed
) {

  def print: String =
    Printer.print(whiteSpacedOpenBracket.print, text.print, attributes.print, whiteSpacedClosingBracket.print)

}

object WhiteSpacedOpenTag {

  def gen(minAttributes: Int, maxAttributes: Int): Gen[Any, WhiteSpacedOpenTag] =
    for {
      start      <- WhiteSpacedBracket.open
      attributes <- WhiteSpacedAttributes.gen(minAttributes, maxAttributes)
      text       <- WhiteSpacedText.gen(
                      minPreSpace = 0,
                      minPostSpace = if (attributes.value.isEmpty) 0 else 1
                    ) // a text should be suffixed by atleast 1 space if there is attribute key-value pair otherwise 0
      stop       <- WhiteSpacedBracket.closed
    } yield WhiteSpacedOpenTag(start, text, attributes, stop)
}
