package zio.config.xml.experimental

import zio.Chunk
import Parsers._
import XmlObject.TagElement
import zio.parser.Parser
import zio.parser.StringParserError

object XmlParser {

  lazy private[config] val tagContent: Parser[String, Char, Option[Chunk[XmlObject]]] =
    xmlParser.repeat
      .orElseEither(textParser.zip(ws).map(Chunk(_)))
      .map(_.merge)
      .optional

  lazy private val xmlParser: Parser[
    String,
    Char,
    XmlObject
  ] =
    ws.zip(openAngular)
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(attributeParser.zip(ws).repeat0)
      .zip(ws)
      .zip(closedAngular)
      .zip(ws)
      .zip(tagContent)
      .zip(ws)
      .zip(closedTag)
      .zip(ws)
      .transformEither { case (name, attributes, xmlObject, (_, _, closedTag)) =>
        if (name == closedTag) {
          Right(
            TagElement(
              name,
              attributes,
              Chunk.fromIterable(xmlObject.toList).flatten
            )
          )
        } else {
          Left(s"Closed tag $closedTag is not the same as open tag $name")
        }
      }

  private[config] def parse(string: String): Either[StringParserError[String], XmlObject] =
    xmlParser.parseString(string)

}
