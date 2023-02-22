package zio.config.xml

import zio.Chunk
import zio.config.xml.XmlObject.Text
import zio.parser.Parser

object Parsers {

  lazy private[config] val UnicodeEmptyCharacters: Chunk[Char] =
    Chunk(
      '\t',
      '\r',
      ' ',
      '\n'
    )

  lazy private[config] val stringLiteral: Parser[String, Char, String] =
    Parser
      .charIn('"')
      .zip(Parser.charNotIn('"').repeat)
      .zip(Parser.charIn('"'))
      .map { case (_, b, _) => b.mkString }

  lazy private[config] val nonWS: Parser[String, Char, Chunk[Char]]    =
    Parser.charNotIn(UnicodeEmptyCharacters: _*).repeat

  lazy private[config] val ws: Parser[String, Char, Unit] =
    Parser.charIn(UnicodeEmptyCharacters: _*).repeat0.unit

  lazy private[config] val tagIdentifier: Parser[String, Char, String] = {
    val invalid =
      Chunk('<', '>') ++ UnicodeEmptyCharacters

    Parser.charNotIn(invalid: _*).repeat.map(_.mkString.trim)
  }

  lazy private[config] val textParser: Parser[String, Char, Text] = {
    val invalid =
      Chunk('<', '>', '=')

    Parser.charNotIn(invalid: _*).repeat.map(s => Text(s.mkString.trim))
  }

  lazy private[config] val attrKeyParser: Parser[String, Char, String] = {
    val invalid =
      Chunk('=') ++ UnicodeEmptyCharacters
    Parser
      .charNotIn(invalid: _*)
      .repeat
      .map(v => v.mkString.trim)
  }

  lazy private[config] val attributeValueParser: Parser[String, Char, String] =
    stringLiteral

  lazy private[config] val attributeParser: Parser[String, Char, (String, String)] =
    attrKeyParser
      .zip(ws)
      .zip(Parser.charIn('=').unit)
      .zip(ws)
      .zip(attributeValueParser)

  lazy private[config] val openAngular: Parser[String, Char, Unit] =
    Parser.charIn('<').unit

  lazy private[config] val closedAngular: Parser[String, Char, Unit] =
    Parser.charIn('>').unit

  lazy private[config] val closedTag: Parser[String, Char, (Char, Char, String)] =
    Parser
      .charIn('<')
      .zip(Parser.charIn('/'))
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(closedAngular)

}
