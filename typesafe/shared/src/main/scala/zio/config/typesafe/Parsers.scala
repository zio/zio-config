package zio.config.typesafe

import zio.Chunk
import zio.parser.Parser

object Parsers {
  lazy private[config] val UnicodeEmptyCharacters: Chunk[Char] =
    Chunk(
      '\t',
      '\r',
      ' ',
      '\n'
    )

  lazy private[config] val openBracket: Parser[String, Char, Unit] =
    Parser.charIn('{').unit

  lazy private[config] val openSquareBracket: Parser[String, Char, Unit] =
    Parser.charIn('[').unit

  lazy private[config] val closedSquareBracket: Parser[String, Char, Unit] =
    Parser.charIn(']').unit

  lazy private[config] val closedBracket: Parser[String, Char, Unit] =
    Parser.charIn('}').unit

  lazy private[config] val keySeparator: Parser[String, Char, Unit] =
    Parser.charIn('=', ':').unit

  lazy private[config] val ws: Parser[String, Char, Unit] =
    Parser.charIn(UnicodeEmptyCharacters: _*).repeat0.unit

  lazy private[config] val keyIdentifier: Parser[String, Char, String] = {
    val invalid =
      Chunk('{', '}') ++ UnicodeEmptyCharacters

    Parser.charNotIn(invalid: _*).repeat.map(_.mkString.trim)
  }

  lazy private[config] val textParserHocon: Parser[String, Char, String] = {
    val invalid =
      Chunk('{', '}', '[', ']', '=', ':', '\n', ',')

    Parser.charNotIn(invalid: _*).repeat.map(s => s.mkString.trim)
  }

}
