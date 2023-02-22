package zio.config.yaml.generators

import zio.Chunk
import zio.config.xml.XmlObject
import zio.config.xml.XmlObject.TagElement
import zio.config.yaml.generators.WhiteSpacedXml.Children
import zio.test.Gen

final case class WhiteSpacedXml(
  openTag: WhiteSpacedOpenTag,
  body: Option[Children],
  closingTag: WhiteSpacedClosingTag
) { self =>

  def printWith(space: Space): String =
    Printer.print(
      self.openTag.print,
      space.print,
      self.printChildren(space),
      space.print,
      self.closingTag.print
    )

  private def printChildren(space: Space): String = {
    def go(randomXml: WhiteSpacedXml): String =
      randomXml.body match {
        case Some(children) =>
          children.value match {
            case Left(whitSpacedText) => whitSpacedText.value
            case Right(randomXmls)    => randomXmls.map(_.printWith(space)).mkString(space.print)
          }
        case None           => space.print
      }

    go(self)
  }

  def emptyChildren: WhiteSpacedXml =
    copy(body = None)

  def toXmlObject: XmlObject = {

    val attributes: Chunk[XmlObject.Attribute] =
      openTag.attributes.value.map { case (randomAttributeWithSpace, _) => randomAttributeWithSpace.toAttribute }

    val children                               =
      body match {
        case Some(value) =>
          value.value match {
            case Left(whiteSpacedText) => Chunk(whiteSpacedText.toXmlObjectText)
            case Right(chunkOfTags)    => chunkOfTags.map(_.toXmlObject)
          }
        case None        => Chunk.empty
      }

    TagElement(openTag.text.value, attributes, children)

  }
}

object WhiteSpacedXml {

  final case class Children(value: Either[WhiteSpacedText, Chunk[WhiteSpacedXml]])

  def gen(minAttributes: Int, maxAttributes: Int): Gen[Any, WhiteSpacedXml] =
    for {
      open     <- WhiteSpacedOpenTag.gen(minAttributes, maxAttributes)
      children <- Gen.option(WhiteSpacedText.gen.map(value => Children(Left(value))))
      closed   <- WhiteSpacedClosingTag.gen(open.text)
    } yield WhiteSpacedXml(open, children, closed)
}
